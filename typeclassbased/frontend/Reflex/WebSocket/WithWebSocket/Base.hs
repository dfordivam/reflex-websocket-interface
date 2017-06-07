{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.WebSocket.WithWebSocket.Base where
  -- ( withWSConnection
  -- , WithWebSocketT (..)
  -- ) where

import Reflex.WebSocket.WithWebSocket.Shared
-- import Reflex.WebSocket.WithWebSocket.Class

import Reflex.Dom hiding (WebSocket,Value, Error)
import qualified Reflex.Dom
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Unique.Tag
import Data.Dependent.Map
import Data.Dependent.Sum
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.ByteString
import Data.Text
import qualified Data.ByteString.Lazy as BSL

import Data.Aeson

type WithWebSocketT ws t m = RequesterT t (IsWebSocketRequest ws) (IsWebSocketResponse ws) m

instance PrimMonad m =>
         PrimMonad (WithWebSocketT ws x m) where
  type PrimState (WithWebSocketT ws x m) = PrimState m
  primitive = lift . primitive

-- instance (MonadWidget t m, PrimMonad m) =>
--          WithWebSocket ws t (WithWebSocketT ws t m) where

getWebSocketResponse
  :: (WebSocketMessage ws req, Monad m, Reflex t, PrimMonad m)
  => Event t req -> WithWebSocketT ws t m (Event t (ResponseT ws req))
getWebSocketResponse req = do
  resp <- requesting $ getWS <$> req
  return $ (\(IsWebSocketResponse b) -> b) <$> resp

getWS :: (WebSocketMessage ws req) => req -> IsWebSocketRequest ws req
getWS req = IsWebSocketRequest (toSum req)

data IsWebSocketRequest ws req where
  IsWebSocketRequest ::
    (WebSocketMessage ws req) => ws -> IsWebSocketRequest ws req

data IsWebSocketResponse ws req where
  IsWebSocketResponse ::
    (WebSocketMessage ws req) =>
      ResponseT ws req -> IsWebSocketResponse ws req


withWSConnection ::
  (MonadWidget t m, PrimState m ~ RealWorld)
  => Text -- URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect
  -> WithWebSocketT ws t m a
  -> m (a, Reflex.Dom.WebSocket t)
withWSConnection url closeEv reconnect wdgt = do
  rec
    let
      respEvMap = getResponseFromBS tagsDyn (_webSocket_recv ws)

    (val, reqEvMap) <- runRequesterT wdgt respEvMap

    let
      bsAndMapEv = getRequestBS reqEvMap
      sendEv = fst <$> bsAndMapEv
      conf = WebSocketConfig sendEv closeEv reconnect
      -- Is it required to remove old tags?
      storeTags :: TagMap ws -> TagMap ws -> TagMap ws
      storeTags = (<>)
    tagsDyn <- foldDyn storeTags Map.empty (snd <$> bsAndMapEv)
    ws <- webSocket url conf
  return (val, ws)

type TagMap ws = Map String (SomeWithFromJSON ws (Tag RealWorld))
type EncodeM ws = Writer ([ByteString], TagMap ws)

data SomeWithFromJSON ws tag where
  ThisWithFromJSON ::
    (WebSocketMessage ws req) =>
    !(tag req) -> SomeWithFromJSON ws tag

doEncode ::
  DSum (Tag RealWorld) (IsWebSocketRequest ws) -> EncodeM ws ()
doEncode (t :=> b@(IsWebSocketRequest a)) = do
  let
    encMes :: IsWebSocketRequest ws req -> Value
    encMes (IsWebSocketRequest a) = toJSON a
  tell
    ( [BSL.toStrict $ encode (show t, encMes b)]
    , Map.singleton (show t) (ThisWithFromJSON t))


-- make a tuple (Tag, Request)
getRequestBS
  :: (Reflex t)
  => Event t (DMap (Tag RealWorld) (IsWebSocketRequest ws)) -> Event t ([ByteString], TagMap ws)
getRequestBS dmapEv = f <$> dmapEv
  where
    f dmap = snd $ runWriter $ mapM_ doEncode (Data.Dependent.Map.toList dmap)

getResponseFromBS
  :: (Reflex t)
  => Dynamic t (TagMap ws)
  -> Event t ByteString
  -> Event t (DMap (Tag RealWorld) (IsWebSocketResponse ws))
getResponseFromBS tagMap bs = fforMaybe inp decodeBSResponse
  where
    inp = attachPromptlyDyn tagMap bs

decodeBSResponse ::
  (TagMap ws, ByteString) -> Maybe (DMap (Tag RealWorld) (IsWebSocketResponse ws))
decodeBSResponse (tagMap,bs) = join $ join $ g <$> taggy
  where
    -- Decode Tag first
    taggy =
      case decodeStrict bs of
        Nothing -> Nothing :: Maybe (String, Value)
        Just (str, rst) -> Just (str, rst)
    -- Given the tag, decode the rest of value
    g (str, rst) = f <$> t
      where
        -- t :: Maybe (SomeWithFromJSON ws (Tag RealWorld))
        t = Map.lookup str tagMap
        f (ThisWithFromJSON t') = decodeValue rst t'
    decodeValue
      :: (WebSocketMessage ws req)
      => Value -> Tag RealWorld req -> Maybe (DMap (Tag RealWorld) (IsWebSocketResponse ws))
    decodeValue bs t =
      case fromJSON bs of
        Error _ -> Nothing
        Success v -> Just (Data.Dependent.Map.singleton t (IsWebSocketResponse v))
