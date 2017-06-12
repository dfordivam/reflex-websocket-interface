{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
