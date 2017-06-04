{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.WebSocket.WithWebSocket.Shared
import qualified Data.Text as T
import Data.Monoid
import Shared
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.WebSockets
import Data.Aeson

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
    (Just (v, Req1 r)) -> toStrict $ encode $ (v :: String, getResponse (Req1 r) r)
    (Just (v, Req2 r)) -> toStrict $ encode $ (v :: String, getResponse (Req2 r) r)
    _ -> error "Cannot decode request"

class GetResponse ws a where
  getResponse :: (WebSocketMessage ws a) => ws -> a -> ResponseT ws a

instance GetResponse Shared.Request Request1 where
  getResponse _ (Request1 t) = Response1 (T.length t)

instance GetResponse Shared.Request Request2 where
  getResponse _ (Request2 (t1,t2)) = Response2 (t1 <> t2)
