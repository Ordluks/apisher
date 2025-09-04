{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Apisher (
  Service (..),
  ServiceRunner,
  RequestHandler,
  API (..),
  runService,
  runAPI,
  sendRequestToSocket',
  sendRequestToSocket,
) where

import Control.Exception (Exception, catch, throw)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Get, Putter, Serialize (..), decode, encode, getInt8, putInt8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

----------------------------------------------

-- | Request head
data RequestHead = RequestHead
  { requestedService :: Int
  -- ^ value to decide for whats service this request is
  , requestBodySize :: Int
  -- ^ count of bytes of request size
  }

instance Serialize (RequestHead) where
  put RequestHead{..} = do
    putInt8 $ fromIntegral $ fromEnum requestedService
    put requestBodySize

  get = RequestHead . toEnum . fromIntegral <$> getInt8 <*> get

-- | Receive and deserialize request head from given socket
recvRequestHead :: Socket -> IO (RequestHead)
recvRequestHead sock = throwOnLeft InvalidRequest . decode <$> recv sock 9

----------------------------------------------

-- | Response head
newtype ResponseHead = ResponseHead
  { responseBodySize :: Int
  -- ^ count of bytes of response size
  }
  deriving (Serialize)

-- | Receive and deserialize response head from given socket
recvResponseHead :: Socket -> IO ResponseHead
recvResponseHead sock = throwOnLeft InvalidResponse . decode <$> recv sock 8

----------------------------------------------

-- | Response body
newtype (Service s) => ResponseBody s = ResponseBody (Either APIError (ResponseContent s)) deriving (Serialize)

-- | Receive and deserialize response body from given socket and length in bytes
recvResponseBody :: (Service s) => Socket -> Int -> IO (ResponseBody s)
recvResponseBody sock size = throwOnLeft InvalidResponse . decode <$> recv sock size

----------------------------------------------

-- | Service
class Service s where
  data RequestContent s
  data ResponseContent s

  -- | Gets type of service by request. Returns "Int" thats mat be converted to API type enum
  typeOfRequest :: RequestContent s -> Int

  serializeRequest :: Putter (RequestContent s)
  deserializeRequest :: Get (RequestContent s)

  serializeResponse :: Putter (ResponseContent s)
  deserializeResponse :: Get (ResponseContent s)

  handleRequest :: RequestHandler s

instance (Service s) => Serialize (RequestContent s) where
  put = serializeRequest
  get = deserializeRequest

instance (Service s) => Serialize (ResponseContent s) where
  put = serializeResponse
  get = deserializeResponse

-- | Makes response content from request content by running some actions
type RequestHandler s = RequestContent s -> IO (ResponseContent s)

-- | Gets encoded request content decode it pass to "RequestHandler" and encode its result
type ServiceRunner = ByteString -> IO ByteString

----------------------------------------------
-- Backend

data (Enum t) => API t = API
  { switchServiceRunner :: t -> ServiceRunner
  , apiOnError :: APIError -> IO ByteString
  }

{- | Gets request head and execute 'switchServiceRunner api' to decide whats "ServiceRunner" will used
to gets and handle request and send response
-}
runAPI :: (Enum t) => API t -> Socket -> IO ()
runAPI api sock = do
  resBodyBytes <-
    catch
      ( do
          (RequestHead{..}) <- recvRequestHead sock
          recv sock requestBodySize >>= switchServiceRunner api (toEnum requestedService)
      )
      (apiOnError api)
  let resHeadBytes = encode $ ResponseHead{responseBodySize = BS.length resBodyBytes}
  sendAll sock (resHeadBytes `BS.append` resBodyBytes)

-- | Creates "ServiceRunner"
runService :: (Service s) => s -> ServiceRunner
runService s reqBytes = encodeResponseBody <$> getRequestHandler s (throwOnLeft InvalidRequest $ decode reqBytes)
 where
  encodeResponseBody = encode . ResponseBody . Right
  getRequestHandler :: (Service s) => s -> RequestHandler s
  getRequestHandler _ = handleRequest

----------------------------------------------
-- Frontend

{- | Send request with given request content to given socket and returns "ResponseBody"
thats can contain "APIError" or response content
-}
sendRequestToSocket' :: (Service s) => RequestContent s -> Socket -> IO (ResponseBody s)
sendRequestToSocket' req sock = do
  let reqBytes = encode req
      reqHeadBytes = encode $ createRequestHead $ BS.length reqBytes
  sendAll sock (reqHeadBytes `BS.append` reqBytes)
  (ResponseHead{..}) <- recvResponseHead sock
  recvResponseBody sock responseBodySize
 where
  createRequestHead :: Int -> RequestHead
  createRequestHead size =
    RequestHead
      { requestedService = typeOfRequest req
      , requestBodySize = size
      }

{- | Send request with given request content to given socket and returns response content.
When "APIError" inside response body it throws it
-}
sendRequestToSocket :: (Service s) => RequestContent s -> Socket -> IO (ResponseContent s)
sendRequestToSocket req sock = do
  (ResponseBody result) <- sendRequestToSocket' req sock
  case result of
    Left err -> throw err
    Right content -> return content

----------------------------------------------

-- | Exception
data APIErrorType
  = InvalidRequest
  | InvalidResponse
  deriving (Enum, Show)

instance Serialize APIErrorType where
  put = putInt8 . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> getInt8

data APIError = APIError
  { apiErrorType :: APIErrorType
  , apiErrorMessage :: String
  }
  deriving (Show)

instance Exception APIError

instance Serialize APIError where
  put (APIError{..}) = do
    put apiErrorType
    put apiErrorMessage

  get = APIError <$> get <*> get

throwOnLeft :: APIErrorType -> Either String a -> a
throwOnLeft errorType (Left msg) = throw $ APIError errorType msg
throwOnLeft _ (Right a) = a