{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflex.WebSocket.WithWebSocket.Shared where

import Data.Aeson

class ( ToJSON ws
      , FromJSON ws
      , ToJSON (ResponseT ws req)
      , FromJSON (ResponseT ws req)
      ) =>
      WebSocketMessage ws req where
  type ResponseT ws req
  toSum :: req -> ws
  fromSum :: ws -> Maybe req
