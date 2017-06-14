{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Reflex.WebSocket.WithWebSocket.Server
  ((:<&>) (..)
  , runHandler
  , Handler (..))
  where

import Reflex.WebSocket.WithWebSocket.Shared
import Data.Aeson

data a :<&> b = a :<&> b
infixr 3 :<&>

data Handler m req a where
  Handler :: (WebSocketMessage req a, Monad m) =>
    req -> (a -> m (ResponseT req a )) -> Handler m req a

class IsValidHandler m req r h where
  runHandlerRec :: req -> r -> h -> m Value

instance (WebSocketMessage req a
  , IsValidHandler m req b c) => IsValidHandler m req (a :<|> b) ((Handler m req a) :<&> c) where
  runHandlerRec req (Terminal a) ((Handler _ h) :<&> _) = h a >>= (\x -> return $ toJSON x)
  runHandlerRec req (Recurse b) (_ :<&> c) = runHandlerRec req b c

instance (WebSocketMessage req a) => IsValidHandler m req a (Handler m req a) where
  runHandlerRec req a (Handler _ h) = h a >>= (\x -> return $ toJSON x)

runHandler :: (IsValidHandler m req req h) => req -> h -> m Value
runHandler req h = runHandlerRec req req h

-- makeHandler
--   :: (WebSocketMessage req a, Monad m)
--   => (a -> m (ResponseT req a)) -> Handler m req a
-- makeHandler = Handler req
--   where req = undefined :: Shared.Request
