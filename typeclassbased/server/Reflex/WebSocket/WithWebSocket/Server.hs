{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Reflex.WebSocket.WithWebSocket.Server
  ((:<&>) (..)
  , handleRequest
  , Handler
  , HandlerWrapper (..)
  , makeHandler)
  where

import Reflex.WebSocket.WithWebSocket.Shared
import Data.Aeson

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

data a :<&> b = a :<&> b
infixr 3 :<&>

type HandlerType m req =
  HandlerRecurs m req req

type family HandlerRecurs (m :: * -> *) req a where
  HandlerRecurs m req (a :<|> b) =
    Handler m req a :<&> (HandlerRecurs m req b)

  HandlerRecurs m req b =
    Handler m req b

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

makeHandler
  :: (WebSocketMessage req a, Monad m)
  => (a -> m (ResponseT req a)) -> Handler m req a
makeHandler = Handler req
  where req = undefined

data HandlerWrapper m req where
  HandlerWrapper :: (FromJSON req, Monad m, IsValidHandler m req req h)
    => h -> HandlerWrapper m req

handleRequest
  :: forall m req.
     (FromJSON req, Monad m)
  => HandlerWrapper m req -> ByteString -> m ByteString
handleRequest (HandlerWrapper h) = handleRequestInternal req h
  where req :: (FromJSON req) => req
        req = undefined

handleRequestInternal
  :: forall m req h.
     (FromJSON req, Monad m, IsValidHandler m req req h)
  => req -> h -> ByteString -> m ByteString
handleRequestInternal _ handler bstr =
  case decodeStrict bstr of
    (Just (v, r)) -> do
      resp <- getResponse r
      return $ toStrict $ encode $ (v :: String, resp)
    _ -> error "Cannot decode request"

  where
    getResponse :: (Monad m) => req -> m Value
    getResponse r = runHandler r handler
