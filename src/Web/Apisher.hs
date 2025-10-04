{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Apisher (
  ServiceType (..),
  Service,
  Backend (..),
  createService,
  backendHandleConnection,
  tryConnectionSendRequest,
  connectionSendRequest,
) where

import Control.Exception (Exception, throw)
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Serialize (Get, Putter, Serialize (..), decode, encode, getInt64be, getInt8, putInt64be, putInt8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

class ServiceType s where
  data RequestContent s
  data ResponseContent s

  serviceTypeId :: s -> Int
  requestToServiceTypeId :: RequestContent s -> Int

  putRequest :: Putter (RequestContent s)
  getRequest :: Get (RequestContent s)

  putResponse :: Putter (ResponseContent s)
  getResponse :: Get (ResponseContent s)

  handleRequest :: RequestContent s -> IO (ResponseContent s)

instance (ServiceType s) => Serialize (RequestContent s) where
  put = putRequest
  get = getRequest

instance (ServiceType s) => Serialize (ResponseContent s) where
  put = putResponse
  get = getResponse

--------------------------------------------------------------------

data Service = Service
  { serviceId :: Int
  , runHandleRequest :: RequestHandlerRunner
  }

type RequestHandlerRunner = ByteString -> IO ByteString

-- | Creates "RequestHandlerRunner"
createRequestHandlerRunner :: (ServiceType s) => s -> RequestHandlerRunner
createRequestHandlerRunner serviceType reqBytes =
  encodeResponseBody <$> getRequestHandler serviceType (throwOnLeft InvalidRequest $ decode reqBytes)
 where
  encodeResponseBody = encode . ResponseBody . Right
  getRequestHandler :: (ServiceType s) => s -> RequestContent s -> IO (ResponseContent s)
  getRequestHandler _ = handleRequest

-- | Creates "Service"
createService :: (ServiceType s) => s -> Service
createService serviceType = Service{..}
 where
  serviceId = serviceTypeId serviceType
  runHandleRequest = createRequestHandlerRunner serviceType

--------------------------------------------------------------------

newtype Backend = Backend {backendServices :: [Service]}

backendHandleConnection :: Backend -> Socket -> IO ()
backendHandleConnection Backend{..} sock = do
  (RequestHead{..}) <- recvRequestHead sock
  let service = findService requestServiceTypeId
  recv sock requestBodySize >>= runHandleRequest service >>= sendAll sock . prependResponseHead
  return ()
 where
  unknownServiceException = ApisherException InvalidRequest "unknown service"
  findService targetServiceId =
    fromMaybe (throw unknownServiceException) $ List.find ((== targetServiceId) . serviceId) backendServices
  prependResponseHead reqBytes = encode (ResponseHead $ BS.length reqBytes) `BS.append` reqBytes

--------------------------------------------------------------------

tryConnectionSendRequest :: (ServiceType s) => Socket -> RequestContent s -> IO (Either ApisherException (ResponseContent s))
tryConnectionSendRequest sock req = do
  sendAll sock (reqHead `BS.append` reqBody)
  (ResponseHead{..}) <- recvResponseHead sock
  responseBodyContent <$> recvResponseBody sock responseBodySize
 where
  reqBody = encode $ RequestBody req
  reqHead = encode $ RequestHead (requestToServiceTypeId req) (BS.length reqBody)

connectionSendRequest :: (ServiceType s) => Socket -> RequestContent s -> IO (ResponseContent s)
connectionSendRequest sock =
  tryConnectionSendRequest sock >=> \case
    Left err -> throw err
    Right res -> return res

--------------------------------------------------------------------

data RequestHead = RequestHead
  { requestServiceTypeId :: Int
  , requestBodySize :: Int
  }

instance Serialize RequestHead where
  put RequestHead{..} = do
    putInt64be $ fromIntegral requestServiceTypeId
    putInt64be $ fromIntegral requestBodySize

  get = RequestHead <$> get <*> get

recvRequestHead :: Socket -> IO RequestHead
recvRequestHead sock = decodeOrThrow InvalidRequest <$> recv sock 16

--------------------------------------------------------------------

newtype (ServiceType s) => RequestBody s = RequestBody (RequestContent s) deriving (Serialize)

--------------------------------------------------------------------

newtype ResponseHead = ResponseHead {responseBodySize :: Int}

instance Serialize ResponseHead where
  put = putInt64be . fromIntegral . responseBodySize
  get = ResponseHead . fromIntegral <$> getInt64be

recvResponseHead :: Socket -> IO ResponseHead
recvResponseHead sock = decodeOrThrow InvalidResponse <$> recv sock 8

--------------------------------------------------------------------

newtype (ServiceType s) => ResponseBody s
  = ResponseBody {responseBodyContent :: Either ApisherException (ResponseContent s)}
  deriving (Serialize)

recvResponseBody :: (ServiceType s) => Socket -> Int -> IO (ResponseBody s)
recvResponseBody sock size = decodeOrThrow InvalidResponse <$> recv sock size

--------------------------------------------------------------------

data ApisherExceptionType
  = InvalidRequest
  | InvalidResponse
  | OtherException
  deriving (Enum, Show)

instance Serialize ApisherExceptionType where
  put = putInt8 . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> getInt8

data ApisherException = ApisherException
  { apisherExceptionType :: ApisherExceptionType
  , apisherExceptionMsg :: String
  }
  deriving (Show)

instance Exception ApisherException

instance Serialize ApisherException where
  put ApisherException{..} = do
    put apisherExceptionType
    put apisherExceptionMsg

  get = ApisherException <$> get <*> get

throwOnLeft :: ApisherExceptionType -> Either String a -> a
throwOnLeft t (Left err) = throw $ ApisherException t err
throwOnLeft _ (Right result) = result

decodeOrThrow :: (Serialize a) => ApisherExceptionType -> ByteString -> a
decodeOrThrow exceptionType = throwOnLeft exceptionType . decode