{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Shared where

import GHC.Generics
import Data.Aeson
import Data.Text

class ( ToJSON ws
      , FromJSON ws
      , ToJSON (ResponseT ws req)
      , FromJSON (ResponseT ws req)
      ) =>
      WebSocketMessage ws req where
  type ResponseT ws req
  toSum :: req -> ws
  fromSum :: ws -> Maybe req

data Request1 = Request1 Text
  deriving (Generic, Show)
data Request2 = Request2 (Text, Text)
  deriving (Generic, Show)

data Response1 = Response1 Int
  deriving (Generic, Show)
data Response2 = Response2 Text
  deriving (Generic, Show)

instance WebSocketMessage Request Request1 where
  type ResponseT Request Request1 = Response1
  toSum = Req1
  fromSum (Req1 r) = Just r
  fromSum _ = Nothing

instance WebSocketMessage Request Request2 where
  type ResponseT Request Request2 = Response2
  toSum = Req2
  fromSum (Req2 r) = Just r
  fromSum _ = Nothing

data Request
  = Req1 Request1
  | Req2 Request2
  deriving (Generic, Show)

instance ToJSON (Request) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request)
instance ToJSON (Request1) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request1)
instance ToJSON (Request2) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request2)

instance ToJSON (Response1) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Response1)

instance ToJSON (Response2) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Response2)
