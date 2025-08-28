{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Apisher (
  APIMethod (..),
  RequestHandler,
  runAPI,
  runAPIMethod,
  sendRequest,
) where

import Control.Exception (Exception, catch, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.Int (Int8)
import Data.Serialize
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

-- | Request head
data RequestHead = RequestHead
  { requestType :: Int8
  -- ^ value to decide for whats API method this request is
  , requestBodySize :: Int
  -- ^ count of bytes of request size
  }

instance Serialize RequestHead where
  put (RequestHead{..}) = do
    put requestType
    putInt64be $ fromIntegral requestBodySize

  get = RequestHead <$> get <*> (fromIntegral <$> getInt64be)

-- | Receive and deserialize request head from given socket
recvRequestHead :: (MonadIO m) => Socket -> m RequestHead
recvRequestHead sock = throwOnLeft InvalidRequestHead . decode <$> liftIO (recv sock 9)

--------------------------------------------------------

-- | Request body
newtype (APIMethod a) => RequestBody a = RequestBody {unwrapRequestContent :: RequestContent a}

instance (APIMethod a) => Serialize (RequestBody a) where
  put = put . unwrapRequestContent
  get = RequestBody <$> get

-- | Receive and deserialize request body from given socket and length in bytes
recvRequestBody :: (APIMethod a, MonadIO m) => Socket -> Int -> m (RequestBody a)
recvRequestBody sock size = throwOnLeft InvalidRequestBody . decode <$> liftIO (recv sock size)

--------------------------------------------------------

-- | Response head
newtype ResponseHead = ResponseHead
  { responseBodySize :: Int
  -- ^ count of bytes of response size
  }

instance Serialize ResponseHead where
  put (ResponseHead{..}) = do
    putInt64be $ fromIntegral responseBodySize

  get = ResponseHead . fromIntegral <$> getInt64be

-- | Receive and deserialize response head from given socket
recvResponseHead :: (MonadIO m) => Socket -> m ResponseHead
recvResponseHead sock = throwOnLeft InvalidResponseHead . decode <$> liftIO (recv sock 8)

--------------------------------------------------------

-- | Response body
data (APIMethod a) => ResponseBody a
  = Fulfilled (ResponseContent a)
  | Rejected APIError

instance (APIMethod a) => Serialize (ResponseBody a) where
  put (Fulfilled content) = put False >> put content
  put (Rejected err) = put True >> put err

  get = do
    doesRejected <- get
    if doesRejected
      then Rejected <$> get
      else Fulfilled <$> get

-- | Receive and deserialize response body from given socket and length in bytes
recvResponseBody :: (APIMethod a, MonadIO m) => Socket -> Int -> m (ResponseBody a)
recvResponseBody sock size = throwOnLeft InvalidResponseBody . decode <$> liftIO (recv sock size)

--------------------------------------------------------

-- | API method
class APIMethod a where
  -- | Main data of the request
  data RequestContent a

  -- | Main data of the response
  data ResponseContent a

  typeOfRequest :: RequestContent a -> Int8

  serializeRequestContent :: Putter (RequestContent a)
  deserializeRequestContent :: Get (RequestContent a)

  serializeResponseContent :: Putter (ResponseContent a)
  deserializeResponseContent :: Get (ResponseContent a)

  handleRequest :: RequestHandler a

instance (APIMethod a) => Serialize (RequestContent a) where
  put = serializeRequestContent
  get = deserializeRequestContent

instance (APIMethod a) => Serialize (ResponseContent a) where
  put = serializeResponseContent
  get = deserializeResponseContent

type RequestHandler a = forall m. (MonadIO m) => RequestContent a -> m (ResponseContent a)

--------------------------------------------------------
-- Back-end

runAPI :: (MonadIO m) => (Int8 -> Int -> Socket -> m ()) -> Socket -> m ()
runAPI api sock = do
  (RequestHead{..}) <- recvRequestHead sock
  api requestType requestBodySize sock

runAPIMethod :: (APIMethod a, MonadIO m) => RequestHandler a -> Int -> Socket -> m ()
runAPIMethod reqHandler reqBodySize sock = do
  resBody <-
    liftIO $
      catch
        (Fulfilled <$> (recvRequestBody sock reqBodySize >>= reqHandler . unwrapRequestContent))
        (\(err :: APIError) -> return $ Rejected err)
  let resHeadBytes = encode $ ResponseHead $ BS.length resBodyBytes
      resBodyBytes = encode resBody
  liftIO $ sendAll sock (resHeadBytes `BS.append` resBodyBytes)

--------------------------------------------------------
-- Front-end

sendRequest :: (APIMethod a, MonadIO m) => RequestContent a -> Socket -> m (ResponseContent a)
sendRequest reqContent sock = do
  let reqHeadBytes =
        encode $
          RequestHead
            { requestType = typeOfRequest reqContent
            , requestBodySize = BS.length reqBodyBytes
            }
      reqBodyBytes = encode $ RequestBody reqContent
  liftIO $
    catch
      ( do
          sendAll sock (reqHeadBytes `BS.append` reqBodyBytes)
          (ResponseHead{..}) <- liftIO $ recvResponseHead sock
          resBody <- recvResponseBody sock responseBodySize
          case resBody of
            Rejected err -> throw err
            Fulfilled content -> return content
      )
      (\(err :: APIError) -> throw err)

--------------------------------------------------------

-- | Exception
data APIErrorType
  = InvalidRequestHead
  | InvalidRequestBody
  | InvalidResponseHead
  | InvalidResponseBody
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