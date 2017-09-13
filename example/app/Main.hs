-- Doing websocket requests from multiple places over a single connection
-- using the Requester class
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Reflex.Dom.Core
import Control.Monad.IO.Class
import Control.Monad.Primitive

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import qualified Data.Text as T
import Data.Monoid

-- Example Code
import Shared

codeToRun
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Shared.Request t m ()
codeToRun = do
  ev1 <- button "Request1"
  ti1 <- textInput def
  respEv1 <- getWebSocketResponse (Request1 <$> tagPromptlyDyn (value ti1) ev1)
  widgetHold (text "Waiting for Response1")
    ((\(Response1 l) -> text ("Length is: " <> T.pack (show l))) <$> respEv1)
  ev2 <- button "Request2"
  ti2 <- textInput def
  respEv2 <- getWebSocketResponse (Request2 <$> tagPromptlyDyn (zipDyn (value ti1) (value ti2)) ev2)
  widgetHold (text "Waiting for Response2")
    ((\(Response2 t) -> text ("Concat string: " <> t)) <$> respEv2)
  return ()

myWidget ::
  (MonadWidget t m, PrimMonad m, PrimState m ~ RealWorld, DomBuilderSpace m ~ GhcjsDomSpace)
  => m ()
myWidget = do
  text "Test WithWebsocket"
  (_,_) <- withWSConnection "ws://127.0.0.1:3000/" never False codeToRun
  return ()

main = mainWidget myWidget
