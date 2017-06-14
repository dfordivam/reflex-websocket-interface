{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Main where

import Reflex.WebSocket.WithWebSocket.Shared
import qualified Data.Text as T
import Data.Monoid
import Shared
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.WebSockets
import Data.Aeson
import Data.Maybe (fromMaybe, catMaybes)

main :: IO ()
main = runServer "127.0.0.1" 3000 app

app c = do
  conn <- acceptRequest c
  let loop = do
        d <- receiveData conn
        print d
        let resp = handleRequest d
        print resp
        sendBinaryData conn resp
        loop
  loop

handleRequest :: ByteString -> ByteString
handleRequest bstr =
  case decodeStrict bstr of
    (Just (v, r)) -> toStrict $ encode $ (v :: String, getResponse' r)
    _ -> error "Cannot decode request"

getResponse' :: Shared.Request -> Value
getResponse' a = head $ catMaybes
  [(toJSON <$> (getResponse1 <$> fromSum a))
  ,(toJSON <$> (getResponse2 <$> fromSum a))]

getResponse1 (Request1 t) = Response1 (T.length t)

getResponse2 (Request2 (t1,t2)) = Response2 (t1 <> t2)

data a :<&> b = a :<&> b

data Handler m req a where
  Handler :: (WebSocketMessage req a, Monad m) =>
    (req -> a -> m (ResponseT req a )) -> Handler m req a

-- Handler m req a -> Handler m req b -> JoinedHandler m req (a :<|> b)
-- Handler m req a -> JoinedHandler m req (b :<|> c) -> JoinedHandler m req (a :<|> (b :<|> c))

-- handler :: (WebSocketMessage req a) => (a :<|> b) -> (a -> m (ResponseT req a)) -> m (Maybe Value)

-- combinedGetResponse = getResponse1 :<&> getResponse2 :<&> getResponse3

-- joinHandler :: (WebSocketMessage req a, WebSocketMessage req b) => (a -> m (ResponseT req a)) :<&> b ->
-- joinHandler (getRespA :<&> (getRespB :<&> other)) = joinHandler other
-- joinHandler (getRespA :<&> (getRespB :<&> other)) = joinHandler other

class IsValidHandler req h where

instance (WebSocketMessage req a
  , IsValidHandler b c) => IsValidHandler (a :<|> b) ((Handler m req a) :<&> c)

instance (WebSocketMessage req a) => IsValidHandler a (Handler m req a)

getResp1 :: Shared.Request -> Request1 -> IO Response1
getResp1 _ (Request1 t) = return $ Response1 (T.length t)

getResp2 :: Shared.Request -> Request2 -> IO Response2
getResp2 _ (Request2 (t1,t2)) = return $ Response2 (t1 <> t2)

runHandler :: (IsValidHandler req h) => req -> h -> m ()
runHandler = undefined

runMain :: Shared.Request -> m ()
runMain val = runHandler val ((Handler getResp1) :<&> (Handler getResp2))
