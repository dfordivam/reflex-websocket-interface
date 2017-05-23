{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reflex.WebSocket.WithWebSocket.Base where
  ( withWSConnection
  ) where

import Reflex.WebSocket.WithWebSocket.Shared
import Reflex.WebSocket.WithWebSocket.Class

import Reflex.Dom hiding (Value, Error)
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

type WithWebSocketT t m = RequesterT t HasToJSON HasFromJSON m

instance PrimMonad m =>
         PrimMonad (WithWebSocketT x m) where
  type PrimState (WithWebSocketT x m) = PrimState m
  primitive = lift . primitive

instance (MonadWidget t m, PrimMonad m) =>
         WithWebSocket t (WithWebSocketT t m) where
  getWebSocketResponse req = do
    resp <- requesting $ HasToJSON <$> req
    return $ (\(HasFromJSON b) -> b) <$> resp

data HasToJSON a where
        HasToJSON ::
            ((FromJSON (WebSocketResponseType a)), ToJSON a) =>
            a -> HasToJSON a

data HasFromJSON a where
        HasFromJSON ::
            (FromJSON (WebSocketResponseType a)) =>
            (WebSocketResponseType a) -> HasFromJSON a


withWSConnection ::
  (MonadWidget t m, PrimState m ~ RealWorld)
  => Text -- URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect
  -> WithWebSocketT t m a
  -> m (a, WebSocket t)
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
      storeTags :: TagMap -> TagMap -> TagMap
      storeTags = (<>)
    tagsDyn <- foldDyn storeTags Map.empty (snd <$> bsAndMapEv)
    ws <- webSocket url conf
  return (val, ws)

type TagMap = Map String (SomeWithFromJSON (Tag RealWorld))
type EncodeM = Writer ([ByteString], TagMap)

data SomeWithFromJSON tag where
        ThisWithFromJSON ::
            (FromJSON (WebSocketResponseType t)) =>
            !(tag t) -> SomeWithFromJSON tag

doEncode ::
  DSum (Tag RealWorld) (HasToJSON) -> EncodeM ()
doEncode (t :=> b@(HasToJSON a)) = do
  -- Can this be simplified and still compiled
  let f (t, HasToJSON a) =
        tell
          ( [BSL.toStrict $ encode (show t, a)]
          , Map.singleton (show t) (ThisWithFromJSON t))
    -- tAndA :: (Tag RealWorld _, _)
      tAndA = (t, b)
  f tAndA

-- make a tuple (Tag, Request)
getRequestBS
  :: (Reflex t)
  => Event t (DMap (Tag RealWorld) HasToJSON) -> Event t ([ByteString], TagMap)
getRequestBS dmapEv = f <$> dmapEv
  where
    f dmap = snd $ runWriter $ mapM_ doEncode (Data.Dependent.Map.toList dmap)

getResponseFromBS
  :: (Reflex t)
  => Dynamic t TagMap
  -> Event t ByteString
  -> Event t (DMap (Tag RealWorld) (HasFromJSON))
getResponseFromBS tagMap bs = fforMaybe inp decodeBSResponse
  where
    inp = attachPromptlyDyn tagMap bs

decodeBSResponse :: (TagMap, ByteString) -> Maybe (DMap (Tag RealWorld) (HasFromJSON))
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
        t :: Maybe (SomeWithFromJSON (Tag RealWorld))
        t = Map.lookup str tagMap
        f (ThisWithFromJSON t') = decodeValue rst t'
    decodeValue
      :: (FromJSON (WebSocketResponseType v))
      => Value -> Tag RealWorld v -> Maybe (DMap (Tag RealWorld) (HasFromJSON))
    decodeValue bs t =
      case fromJSON bs of
        Error _ -> Nothing
        Success v -> Just (Data.Dependent.Map.singleton t (HasFromJSON v))
