{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Serialize
import Network.Apisher (APIMethod (..), RequestHandler, runAPI, runAPIMethod, sendRequest)
import Network.Run.TCP (runTCPClient, runTCPServer)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data APIMethodName
  = ExampleMethod1
  | ExampleMethod2
  deriving (Enum)

fromEnumN :: (Num n, Enum a) => a -> n
fromEnumN = fromIntegral . fromEnum

toEnumN :: (Integral n, Enum a) => n -> a
toEnumN = toEnum . fromIntegral

data Example1

instance APIMethod Example1 where
  data RequestContent Example1 = Example1Request {exm1ReqMsg :: String}
  data ResponseContent Example1 = Example1Response {exm1ResMsg :: String}

  typeOfRequest = const $ fromEnumN ExampleMethod1

  serializeRequestContent = put . exm1ReqMsg
  deserializeRequestContent = Example1Request <$> get

  serializeResponseContent = put . exm1ResMsg
  deserializeResponseContent = Example1Response <$> get

  handleRequest = return . Example1Response . (++ " response") . exm1ReqMsg

data Example2

instance APIMethod Example2 where
  data RequestContent Example2 = Example2Request {exm2ReqMsg :: String}
  data ResponseContent Example2 = Example2Response {exm2ResMsg :: String}

  typeOfRequest = const $ fromEnumN ExampleMethod1

  serializeRequestContent = put . exm2ReqMsg
  deserializeRequestContent = Example2Request <$> get

  serializeResponseContent = put . exm2ResMsg
  deserializeResponseContent = Example2Response <$> get

  handleRequest = return . Example2Response . exm2ReqMsg

deriving instance Eq (ResponseContent Example1)
deriving instance Show (ResponseContent Example1)

deriving instance Eq (ResponseContent Example2)
deriving instance Show (ResponseContent Example2)

runServer :: IO ()
runServer = runTCPServer Nothing "8080" $ runAPI $ \n -> case toEnumN n of
  ExampleMethod1 -> runAPIMethod (handleRequest :: RequestHandler Example1)
  ExampleMethod2 -> runAPIMethod (handleRequest :: RequestHandler Example2)

testSendRequest ::
  (APIMethod a, Eq (ResponseContent a), Show (ResponseContent a)) =>
  TestName ->
  RequestContent a ->
  ResponseContent a ->
  TestTree
testSendRequest testMsg req expectedRes = testCase testMsg $
  runTCPClient "127.0.0.1" "8080" $ \sock -> do
    res <- sendRequest req sock
    assertEqual "response content must be equal expected response content" expectedRes res

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testSendRequest
        "send example 1 request"
        (Example1Request "example 1")
        (Example1Response "example 1 response")
    , testSendRequest
        "send example 2 request"
        (Example2Request "example 2")
        (Example2Response "example 2 response")
    ]

main :: IO ()
main = do
  void $ forkIO runServer
  defaultMain tests