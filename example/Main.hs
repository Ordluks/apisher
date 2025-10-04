{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Serialize

import Network.Run.TCP (runTCPClient, runTCPServer)
import Web.Apisher

data ExampleAPI
  = ExampleService1
  | ExampleService2
  deriving (Enum)

data Example1 = Example1

instance ServiceType Example1 where
  data RequestContent Example1 = Example1Request {ex1Req :: String}
  data ResponseContent Example1 = Example1Response {ex1Res :: String}

  serviceTypeId = const $ fromEnum ExampleService1
  requestToServiceTypeId = const $ fromEnum ExampleService1

  putRequest = put . ex1Req
  getRequest = Example1Request <$> get

  putResponse = put . ex1Res
  getResponse = Example1Response <$> get

  handleRequest = return . Example1Response . (++ " response") . ex1Req

data Example2 = Example2

instance ServiceType Example2 where
  data RequestContent Example2 = Example2Request {ex2Req :: String}
  data ResponseContent Example2 = Example2Response {ex2Res :: String}

  serviceTypeId = const $ fromEnum ExampleService2
  requestToServiceTypeId = const $ fromEnum ExampleService2

  putRequest = put . ex2Req
  getRequest = Example2Request <$> get

  putResponse = put . ex2Res
  getResponse = Example2Response <$> get

  handleRequest = return . Example2Response . (++ " response") . ex2Req

deriving instance Eq (ResponseContent Example1)
deriving instance Show (ResponseContent Example1)

deriving instance Eq (ResponseContent Example2)
deriving instance Show (ResponseContent Example2)

backend :: Backend
backend =
  Backend
    [ createService Example1
    , createService Example2
    ]

runServer :: IO ()
runServer = runTCPServer Nothing "8080" $ backendHandleConnection backend

sendRequest :: (ServiceType s) => RequestContent s -> IO (ResponseContent s)
sendRequest = runTCPClient "127.0.0.1" "8080" . flip connectionSendRequest

main :: IO ()
main = do
  void $ forkIO runServer
  sendRequest (Example1Request "example 1") >>= print
  sendRequest (Example2Request "example 2") >>= print
