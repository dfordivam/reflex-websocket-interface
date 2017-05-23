{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.WebSocket.WithWebSocket.Class where

import Reflex.WebSocket.WithWebSocket.Shared

import Reflex.Dom.Class
import Reflex.Dom.Builder.Class
import Reflex.PostBuild.Class
import Reflex.PerformEvent.Class
import Reflex.Class
import Reflex.TriggerEvent.Class
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import GHCJS.DOM.Types (MonadJSM)

import Data.Aeson

class ( MonadJSM m
      , MonadJSM (Performable m)
      , HasJSContext m
      , PerformEvent t m
      , TriggerEvent t m
      , PostBuild t m
      , DomBuilder t m
      , MonadHold t m
      , MonadFix m
      ) =>
      WithWebSocket t m | m -> t where
  getWebSocketResponse
    :: (ToJSON a, FromJSON (WebSocketResponseType a), WithWebSocket t m)
    => Event t a -> m (Event t (WebSocketResponseType a))
