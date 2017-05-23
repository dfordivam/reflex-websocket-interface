Important things in a good websocket interface
1. Request -> Response dependency
   - Functional dependence using type class
   - Type family
   - GADT with tags
2. Completeness of server handler - There should be a handler for each type of request. Compiler should warn/error if handler is missing
3. Client side monadic interface - The requests come from different parts of the application and their responses need to be routed back to the corresponding place.
getResponse :: Event t request -> m (Event t response)
4. Server side SOP interface - A single handler has to take care of multiple request types
    handleRequest (MonadIO m) :: request -> m (response)
