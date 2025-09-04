{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Serialize

import Network.Run.TCP (runTCPClient, runTCPServer)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Web.Apisher

data ExampleAPI
  = ExampleService1
  | ExampleService2
  deriving (Enum)

data Example1 = Example1

instance Service Example1 where
  data RequestContent Example1 = Example1Request {ex1Req :: String}
  data ResponseContent Example1 = Example1Response {ex1Res :: String}

  typeOfRequest = const $ fromEnum ExampleService1

  serializeRequest = put . ex1Req
  deserializeRequest = Example1Request <$> get

  serializeResponse = put . ex1Res
  deserializeResponse = Example1Response <$> get

  handleRequest = return . Example1Response . (++ " response") . ex1Req

data Example2 = Example2

instance Service Example2 where
  data RequestContent Example2 = Example2Request {ex2Req :: String}
  data ResponseContent Example2 = Example2Response {ex2Res :: String}

  typeOfRequest = const $ fromEnum ExampleService2

  serializeRequest = put . ex2Req
  deserializeRequest = Example2Request <$> get

  serializeResponse = put . ex2Res
  deserializeResponse = Example2Response <$> get

  handleRequest = return . Example2Response . (++ " response") . ex2Req

deriving instance Eq (ResponseContent Example1)
deriving instance Show (ResponseContent Example1)

deriving instance Eq (ResponseContent Example2)
deriving instance Show (ResponseContent Example2)

api :: API ExampleAPI
api =
  API
    { switchServiceRunner = \case
        ExampleService1 -> runService Example1
        ExampleService2 -> runService Example2
    , apiOnError = return . encode
    }

runServer :: IO ()
runServer = runTCPServer Nothing "8080" $ runAPI api

testSendRequest ::
  (Service s, Eq (ResponseContent s), Show (ResponseContent s)) =>
  TestName ->
  RequestContent s ->
  ResponseContent s ->
  TestTree
testSendRequest testName req expectedRes = testCase testName $
  runTCPClient "127.0.0.1" "8080" $ \sock -> do
    res <- sendRequestToSocket req sock
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