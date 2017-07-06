# Websocket Interface for Reflex applications

This repository consists of three packages to be used together, along with reflex as Dom builder, to do websocket communication.

## Highlights

Uses type operators to create the request and `Generic` to avoid writing most of the server side boiler plate code.

1. Request -> Response

   Type-checker make sure your client and server APIs are in sync.

2. Completeness of server handler

   The Type-checker also make sure that all the requests are handled by the server side code.

3. Client side monadic interface with routing of response event.

   In the client application the requests come from different parts of the DOM and their responses need to be routed back to the same place.
   This is all taken care by the library code and the user interface is as simple as calling an API

   `getResponse :: Event t request -> m (Event t response)`

## Usage

See the code in example folder for more details.

1. Shared code

   Create a shared code which has the websocket message type using the package reflex-websocket-interface-shared

   Define the types of all the requests and their corresponding responses.
   Also derive Generic instance  of all the individual request and response types.
   This can be used to automatically derive the ToJSON and FromJSON instances also.

   The collection of all websocket requests which can happen over a single connection are grouped together using the type operator (:<|>).
   In the rest of document this type is called referred as the request-type.

   ```
   type Request = Request1 :<|> Request2 :<|> Request3

   data Request1 = Request1 Text
     deriving (Generic, Show)
   data Request2 = Request2 (Text, Text)
     deriving (Generic, Show)
   data Request3 = Request3 [Text]
     deriving (Generic, Show)

   data Response1 = Response1 Int
     deriving (Generic, Show)
   data Response2 = Response2 Text
     deriving (Generic, Show)
   data Response3 = Response3 (Text, Int)
     deriving (Generic, Show)

   ```

   Next specify the WebSocketMessage instances for each of individual requests contained in the request-type

   ```
   instance WebSocketMessage Request Request1 where
     type ResponseT Request Request1 = Response1

   instance WebSocketMessage Request Request2 where
     type ResponseT Request Request2 = Response2

   instance WebSocketMessage Request Request3 where
     type ResponseT Request Request3 = Response3
   ```

2. Frontend

   In the reflex application use the reflex-websocket-interface package.

   Use the `getWebSocketResponse` API along with the other DomBuilder code to create the widget in the `WithWebSocketT` monad.

   ```
     -- req1 :: Event t Request1
     -- respEv1 :: Event t Response1
     respEv1 <- getWebSocketResponse req1
   ```

   and specify this widget in `withWSConnection` API along with the websocket url to run the widget using the websocket connection.

   ```
     (retVal,wsConn) <- withWSConnection
        url wsCloseEvent doRecconectBool widgetCode
   ```

3. Server

   Use the reflex-websocket-interface-server package

   Specify all the handlers like this (m can be any monad, Use Identity monad if the handler code is pure)

   ```
   handleRequest1 :: (Monad m) => Request1 -> m Response1
   handleRequest2 :: (Monad m) => Request2 -> m Response2
   ```

   Create a main handler using all the individual handler using the type operator (:<&>) and the makeHandler API.
   You need to specify the request-type explicitly in the HandlerWrapper and makeHandler like this

   ```
   handler :: HandlerWrapper IO Request
   handler = HandlerWrapper $
     h handleRequest1
     :<&> h handleRequest2
     where
       h :: (WebSocketMessage Request a, Monad m)
         => (a -> m (ResponseT Request a))
         -> Handler m Request a
       h = makeHandler
   ```

   Use this handler in the handleRequest API, also specify the bytestring received from the websocket connection.
   This API will run the appropriate handler based on the request type and encode the response back in bytestring.

   ```
   -- resp :: Bytestring
   resp <- handleRequest handler bsRecieved
   ```

Here I have used the aeson package for serialisation. To use some other serialisation package the library code will need slight modifications, but should work.
