{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , getWebSocketResponse
  ) where

import Reflex.Dom.WebSocket.Message

import Reflex.Dom.Core hiding (WebSocket,Value, Error)
import qualified Reflex.Dom.Core as Reflex.Dom
import Data.Dependent.Sum
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.ByteString
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL

import Control.Monad.State.Strict

import Data.Aeson

type WithWebSocketT ws t m = RequesterT t (IsWebSocketRequest ws) (IsWebSocketResponse ws) m

data IsWebSocketRequest ws req where
  IsWebSocketRequest ::
    (WebSocketMessage ws req) => ws -> IsWebSocketRequest ws req

data IsWebSocketResponse ws req where
  IsWebSocketResponse ::
    (WebSocketMessage ws req) =>
      ResponseT ws req -> IsWebSocketResponse ws req

newtype TagKey = TagKey { unTagKey :: Int }
  deriving (Show, Eq, Ord, Enum)

type TagMap ws = Map TagKey (SomeRequesterDataKey ws)

-- Similar to the "Some" with additional constaints
data SomeRequesterDataKey ws where
  ThisRequesterDataKey ::
    (WebSocketMessage ws req) =>
    RequesterDataKey req -> SomeRequesterDataKey ws

getWebSocketResponse
  :: (WebSocketMessage ws req, Monad m, Reflex t)
  => Event t req -> WithWebSocketT ws t m (Event t (ResponseT ws req))
getWebSocketResponse req = do
  resp <- requesting $ getWS <$> req
  return $ (\(IsWebSocketResponse b) -> b) <$> resp
  where
    getWS :: (WebSocketMessage ws req) => req -> IsWebSocketRequest ws req
    getWS req = IsWebSocketRequest (toSum req)

withWSConnection ::
  (MonadWidget t m)
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
      sendEv = fst <$> bsAndMapEv
      conf = WebSocketConfig sendEv closeEv reconnect

      bsAndMapEv = getRequestBS <$> attachPromptlyDyn tagsDyn reqEvMap
    tagsDyn <- foldDyn (<>) Map.empty (snd <$> bsAndMapEv)
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
          [] -> 0
          (k':_) -> unTagKey $ fst $ k'
        k = TagKey $ lastKey + 1

        encMes :: IsWebSocketRequest ws req -> Value
        encMes (IsWebSocketRequest a) = toJSON a
        bs = BSL.toStrict $ encode (show k, encMes b)
      modify (\(bsl,m) -> (bs:bsl, Map.insert k (ThisRequesterDataKey reqKey) m))

getResponseFromBS
  :: (Reflex t)
  => Dynamic t (TagMap ws)
  -> Event t ByteString
  -> Event t (RequesterData (IsWebSocketResponse ws))
getResponseFromBS tagMap bs = fforMaybe inp decodeBSResponse
  where
    inp = attachPromptlyDyn tagMap bs

decodeBSResponse ::
  (TagMap ws, ByteString) -> Maybe (RequesterData (IsWebSocketResponse ws))
decodeBSResponse (tagMap,bs) = join $ join $ decodeValue <$> decodeTag
  where
    -- Decode Tag first
    decodeTag =
      case decodeStrict bs of
        Nothing -> Nothing :: Maybe (TagKey, Value)
        Just (val, rst) -> Just (TagKey val, rst)

    -- Given the tag, decode the rest of value
    decodeValue (tagMapKey, rst) = f <$> t
      where
        t = Map.lookup tagMapKey tagMap
        f (ThisRequesterDataKey t') = decodeValue2 rst t'

    decodeValue2
      :: (WebSocketMessage ws req)
      => Value
      -> RequesterDataKey req
      -> Maybe (RequesterData (IsWebSocketResponse ws))
    decodeValue2 bs reqKey =
      case fromJSON bs of
        Error _ -> Nothing
        Success v -> Just (singletonRequesterData reqKey (IsWebSocketResponse v))
