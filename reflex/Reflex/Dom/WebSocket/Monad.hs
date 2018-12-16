{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- The reflex side code need to do the following
-- 1. Capture all the Requent events
-- 2. Encode the request to JSON and send on websocket
--    Tag the request, so that we know the response is for which request
--
-- We need the tag to
--   a. Decode the response ie get the type of the response
--      -
--   b. send the response to the origin of request
--      - For this we need to store the RequesterDataKey

module Reflex.Dom.WebSocket.Monad
  ( withWSConnection
  , WithWebSocketT
  , HasWebSocket
  , IsWebSocketRequest(..)
  , IsWebSocketResponse(..)
  , getWebSocketResponse
  ) where

import Reflex.Dom.WebSocket.Message

import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict
import Data.Aeson
import Data.Align
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Dependent.Sum
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.These
import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom.Core hiding (Error, Value, WebSocket)
import qualified Reflex.Dom.Core as Reflex.Dom

type WithWebSocketT ws t m = RequesterT t (IsWebSocketRequest ws) (IsWebSocketResponse ws) m

type HasWebSocket t msg m = (Requester t m, Request m ~ IsWebSocketRequest msg, Response m ~ IsWebSocketResponse msg)

data IsWebSocketRequest ws req where
  IsWebSocketRequest ::
    (WebSocketMessage ws req) => ws -> IsWebSocketRequest ws req

data IsWebSocketResponse ws req where
  IsWebSocketResponse ::
    (WebSocketMessage ws req) =>
      ResponseT ws req -> IsWebSocketResponse ws req

newtype TagKey = TagKey { unTagKey :: Int }
  deriving (Show, Eq, Ord, Enum, ToJSON, FromJSON)

type TagMap ws = Map TagKey (SomeRequesterDataKey ws)

-- Similar to the "Some" with additional constaints
data SomeRequesterDataKey ws where
  ThisRequesterDataKey ::
    (WebSocketMessage ws req) =>
    RequesterDataKey req -> SomeRequesterDataKey ws

getWebSocketResponse
  :: (WebSocketMessage ws req, HasWebSocket t ws m)
  => Event t req -> m (Event t (ResponseT ws req))
getWebSocketResponse req = do
  resp <- requesting $ getWS <$> req
  return $ (\(IsWebSocketResponse b) -> b) <$> resp
  where
    getWS :: (WebSocketMessage ws req) => req -> IsWebSocketRequest ws req
    getWS req = IsWebSocketRequest (toSum req)

withWSConnection :: forall t m ws a.
  ( Reflex t, Monad m, MonadHold t m, MonadFix m
  , HasJSContext m, MonadJSM m, MonadJSM (Performable m)
  , PerformEvent t m, PostBuild t m, TriggerEvent t m )
  => Text -- URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect
  -> WithWebSocketT ws t m ByteString
  -> m (ByteString, Reflex.Dom.WebSocket t)
withWSConnection url closeEv reconnect wdgt = do
  let
      foldFun :: These (TagMap ws) (TagMap ws) -> TagMap ws -> TagMap ws
      foldFun (This r) m    = Map.difference m r
      foldFun (That a) m    = Map.union m a
      foldFun (These r a) m = Map.difference (Map.union m a) r

  rec
    let
      respEvMap = getResponseFromBS tagsDyn (_webSocket_recv ws)
    (val, reqEvMap) <- runRequesterT wdgt (snd <$> respEvMap)

    let
      sendEv :: Event t [ByteString]
      sendEv = fst <$> bsAndMapEv
      conf = WebSocketConfig sendEv closeEv reconnect []

      bsAndMapEv = getRequestBS <$> attach (current tagsDyn) reqEvMap
      removeKeyEv = fst <$> respEvMap
      addKeyEv = snd <$> bsAndMapEv
      keyEv = align removeKeyEv addKeyEv

    tagsDyn <- foldDyn foldFun Map.empty keyEv
    ws <- webSocket url conf
  return (val, ws)

-- Code to encode and decode the messages
-----------------------------------------

type EncodeM ws = State ([ByteString], TagMap ws)

getRequestBS
  :: (TagMap ws, RequesterData (IsWebSocketRequest ws))
  -> ([ByteString], TagMap ws)
getRequestBS (prevTagMap, reqData) =
  snd $ runState (mapM_ doEncode (requesterDataToList reqData)) ([], prevTagMap)
  where
    doEncode ::
      DSum RequesterDataKey (IsWebSocketRequest ws) -> EncodeM ws ()
    doEncode (reqKey :=> b@(IsWebSocketRequest a)) = do
      (s :: ([ByteString], TagMap ws)) <- get
      let
        lastKey = case (Map.toDescList $ snd s) of
          []     -> 0
          (k':_) -> unTagKey $ fst $ k'
        k = TagKey $ lastKey + 1

        encMes :: IsWebSocketRequest ws req -> Value
        encMes (IsWebSocketRequest a) = toJSON a
        bs = BSL.toStrict $ encode (k, encMes b)
      modify (\(bsl,m) -> (bs:bsl, Map.insert k (ThisRequesterDataKey reqKey) m))

getResponseFromBS
  :: (Reflex t)
  => Dynamic t (TagMap ws)
  -> Event t ByteString
  -> Event t (TagMap ws, RequesterData (IsWebSocketResponse ws))
getResponseFromBS tagMap bs = fforMaybe inp decodeBSResponse
  where
    inp = attach (current tagMap) bs

decodeBSResponse ::
  (TagMap ws, ByteString) -> Maybe (TagMap ws, RequesterData (IsWebSocketResponse ws))
decodeBSResponse (tagMap,bs) = join $ join $ decodeValue <$> decodeTag
  where
    -- Decode Tag first
    decodeTag =
      case decodeStrict bs of
        Nothing         -> Nothing :: Maybe (TagKey, Value)
        Just (val, rst) -> Just (val, rst)

    -- Given the tag, decode the rest of value
    decodeValue (tagMapKey, rst) = f <$> t
      where
        t = Map.lookup tagMapKey tagMap
        f (ThisRequesterDataKey t') = decodeValue2 tagMapKey rst t'

    decodeValue2
      :: (WebSocketMessage ws req)
      => TagKey
      -> Value
      -> RequesterDataKey req
      -> Maybe (TagMap ws, RequesterData (IsWebSocketResponse ws))
    decodeValue2 tagKey bs reqKey =
      case fromJSON bs of
        Error _ -> Nothing
        Success v -> Just (Map.singleton tagKey $ ThisRequesterDataKey reqKey,
                           singletonRequesterData reqKey (IsWebSocketResponse v))
