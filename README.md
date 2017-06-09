## Important things in a good websocket interface
1. Request -> Response dependency

2. Completeness of server handler - There should be a handler for each type of request. Compiler should warn/error if handler is missing

3. Client side monadic interface - The requests come from different parts of the application and their responses need to be routed back to the corresponding place.

`getResponse :: Event t request -> m (Event t response)`

4. Server side SOP interface - A single handler has to take care of multiple request types
    
    `handleRequest (MonadIO m) :: request -> m (response)`

## Implementations

1. typeclassbased -
   This approach uses a user-defined sum-type for requests.
   
   It solves all the requirements defined above
   
   This requires a good amount of boilerplate code from user
   

```

class ( ToJSON ws
      , FromJSON ws
      , ToJSON (ResponseT ws req)
      , FromJSON (ResponseT ws req)
      ) =>
      WebSocketMessage ws req where
  type ResponseT ws req
  toSum :: req -> ws
  fromSum :: ws -> Maybe req

```
The request has to be a sum type like this
```
data Request
  = Req1 Request1
  | Req2 Request2
  deriving (Generic, Show)
```
And we need to create instances of WebSocketMessage like these
```
instance WebSocketMessage Request Request1 where
  type ResponseT Request Request1 = Response1
  toSum = Req1
  fromSum (Req1 r) = Just r
  fromSum _ = Nothing

instance WebSocketMessage Request Request2 where
  type ResponseT Request Request2 = Response2
  toSum = Req2
  fromSum (Req2 r) = Just r
  fromSum _ = Nothing
```



   - Functional dependence using type class

     ```
     class IsMessage request response | request -> response
     ```

   - Type family

   ```
   type family Response request

   ```

   - GADT with tags

   ```
   data Tag1
   data Tag2

   data Request tag where
     ReqTag1 :: ReqTag1T -> Request Tag1
     ReqTag2 :: ReqTag2T -> Request Tag2
   data Response tag where
     ResTag1 :: ResTag1T -> Response Tag1
     ResTag2 :: ResTag2T -> Response Tag2

   getResponse :: Event t (Request tag) -> m (Event t (Response tag))
   ```
