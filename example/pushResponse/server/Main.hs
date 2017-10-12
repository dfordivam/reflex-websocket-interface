{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Reflex.Dom.WebSocket.Server
import Reflex.Dom.WebSocket.Message
import qualified Data.Text as T
import Data.Monoid
import Shared
import Network.WebSockets

main :: IO ()
main = runServer "127.0.0.1" 3000 app

type AppM = StateT AppState IO

data AppState = AppState
  { appStateIntVal :: Int
  , appStateTextMVar :: MVar Text
  }

app c = do
  conn <- acceptRequest c
  let loop = do
        d <- liftIO $ receiveData conn
        liftIO $ print d
        handlePushRequest
          (\m -> print m >> sendBinaryData conn m) d
        loop
  mVar <- newEmptyMVar
  runStateT loop (AppState 0 mVar)

handler :: PushHandlerWrapper AppM Shared.Request
handler = PushHandlerWrapper a b
  where
    h :: (WebSocketMessage Shared.Request a, Monad m)
      => (a -> m (ResponseT Shared.Request a))
      -> Handler m Shared.Request a
    h = makeHandler

    g :: (WebSocketMessage Shared.Request a, Monad m)
      => (a -> (ResponseT Shared.Request a -> IO ()) -> m ())
      -> Handler m Shared.Request a
    g = makePushHandler

    a = h getResp1
      :<&> h getResp2
      :<&> h getResp3
    b = g sendResp4
      :<&> g sendResp5
      :<&> g sendResp6

getResp1 :: Request1 -> AppM Response1
getResp1 (Request1 t) = return $ Response1 (T.length t)

getResp2 :: Request2 -> AppM Response2
getResp2 (Request2 (t1,t2)) = return $ Response2 (t1 <> t2)

getResp3 :: Request3 -> AppM Response3
getResp3 (Request3 ts) = return $ Response3 ("Length", length ts)

sendResp4 :: Request4 -> (Response4 -> IO ()) -> AppM ()
sendResp4 (Request4 t) sendFun = liftWith (\run ->
  bracket
    (putStrLn "Started sendResp4")
    (putStrLn "Finished sendResp4")
    (run . sendResp4Main)) >>= restoreT . return
  where
    sendResp4Main :: AppM ()
    sendResp4Main = do
      let loop = do
            val <- gets appStateIntVal
            put (val + 1)
            sendFun val
            delay 100000
            loop
      loop

sendResp5 :: Request5 -> (Response5 -> IO ()) -> AppM ()
sendResp5 (Request5 t) sendFun = liftWith (\run ->
  bracket
    (putStrLn "Started sendResp5")
    (putStrLn "Finished sendResp5")
    (run . sendResp5Main)) >>= restoreT . return
  where
    sendResp5Main :: AppM ()
    sendResp5Main = do
      mVar <- gets appStateTextMVar
      let loop = do
            val <- readMVar mVar
            sendFun val
            loop
      loop

sendResp6 :: Request6 -> (Response6 -> IO ()) -> AppM ()
sendResp6 (Request6 t) sendFun = liftWith (\run ->
  bracket
    (putStrLn "Started sendResp6")
    (putStrLn "Finished sendResp6")
    (run . sendResp6Main)) >>= restoreT . return
  where
    sendResp6Main :: AppM ()
    sendResp6Main = do
            sendFun ("v1", "v2")
            delay 100000
            sendFun ("v3", "v4")
