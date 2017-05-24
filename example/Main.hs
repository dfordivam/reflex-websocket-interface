-- Doing websocket requests from multiple places over a single connection
-- using the Requester class
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Reflex.Dom
import Control.Monad.IO.Class
import Control.Monad.Primitive

import Reflex.WebSocket.WithWebSocket.Class
import Reflex.WebSocket.WithWebSocket.Base
import Reflex.WebSocket.WithWebSocket.Shared

-- Example Code
type instance WebSocketResponseType String = Char
type instance WebSocketResponseType Char = [Char]

codeToRun ::
  (WithWebSocket t m)
  => m ()
codeToRun = do
  ev <- button "hello"
  respEv <- getWebSocketResponse ('f' <$ ev)
  d <- foldDyn (++) "" respEv
  display d
  return ()

myWidget ::
  (MonadWidget t m, PrimMonad m, PrimState m ~ RealWorld)
  => m ()
myWidget = do
  text "Test WithWebsocket"
  (_,_) <- withWSConnection "ws://echo.websocket.org/" never False codeToRun
  return ()

main = mainWidget myWidget
