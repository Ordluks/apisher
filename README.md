# APIsher

It is library provides you a simple engine to create fast and light-weight server API thats can work over TCP or UDP protocols.

## Terminology

### Request and response

**Request** is message from client to server. It is *contains* all information and data about action that server needs to do. **Response** sends from server to client after request handling ends and contains some data requested by client.

### API method

**API method** is an action that your server backend can do for your frontend. **Request content**, **response content** and **request handler** must be implemented for any method. **Request handler** is function thats takes **request content** run some action and return **response content**.

```haskell
forall a . (APIMethod a) => RequestContent a -> IO (ResponseContent) -- RequestHandler type reference
```

## Tutorial
`APIMethod` types family class has `typeOfRequest` method thats implementation must return uniquer request type as `Int8`.
You needs to implement serialization and deserialization methods for `RequestContent` and `ResponseContent`. See  [documentation for cereal library on Hackage](https://hackage.haskell.org/package/cereal). `handleRequest` is for `RequestHandler`.

Function `runAPI` takes function thats takes request type, request body size, socket and return empty monad `IO` as first argument and socket as second argument and return empty `IO`. `runAPIMethod` takes `RequestHandler`, request body size, socket and return empty `IO`. You can combine thats two function.

```haskell
runAPI :: (Int8 -> Int -> Socket -> IO ()) -> Socket -> IO ()
runAPIMethod :: (APIMethod a) => RequestHandler a -> Int -> Socket -> IO ()
```

```haskell
-- Recommends define data type like this and derive Enum
-- It is helps you manage request type more easy
data APIMethodName
  = ExampleMethod1
  | ExampleMethod2
  deriving (Enum)


data Example1
instance APIMethod Example1 where

data Example2
instance APIMethod Example2 where

runServer :: IO ()
runServer = runTCPServer Nothing "8080" $ runAPI $ \n -> case fromIntegral $ toEnum n of
  ExampleMethod1 -> runAPIMethod (handleRequest :: RequestHandler Example1)
  ExampleMethod2 -> runAPIMethod (handleRequest :: RequestHandler Example2)
```

And run sendRequest from your frontend app.

```haskell
client = runTCPClient "127.0.0.1" "8080" $ \sock -> do
    let req = Example1Request "..."
    res <- sendRequest req sock -- as type of req is "RequestContent Example1" type of res will be "Response Content Example1"
    return ()
```

## Example

See the `test/Spec.hs` for basic example.
