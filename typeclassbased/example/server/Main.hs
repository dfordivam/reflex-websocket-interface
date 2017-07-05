{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Reflex.WebSocket.WithWebSocket.Shared
import Reflex.WebSocket.WithWebSocket.Server
import qualified Data.Text as T
import Data.Monoid
import Shared
import Network.WebSockets

main :: IO ()
main = runServer "127.0.0.1" 3000 app

app c = do
  conn <- acceptRequest c
  let loop = do
        d <- receiveData conn
        print d
        resp <- handleRequest handler d
        print resp
        sendBinaryData conn resp
        loop
  loop

handler :: HandlerWrapper IO Shared.Request
handler = HandlerWrapper $
  h getResp1
  :<&> h getResp2
  :<&> h getResp3
  where
    h :: (WebSocketMessage Shared.Request a, Monad m)
      => (a -> m (ResponseT Shared.Request a))
      -> Handler m Shared.Request a
    h = makeHandler

getResp1 :: Request1 -> IO Response1
getResp1 (Request1 t) = return $ Response1 (T.length t)

getResp2 :: Request2 -> IO Response2
getResp2 (Request2 (t1,t2)) = return $ Response2 (t1 <> t2)

getResp3 :: Request3 -> IO Response3
getResp3 (Request3 ts) = return $ Response3 ("Length", length ts)
