{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared where

import Reflex.Dom.WebSocket.Message

import GHC.Generics
import Data.Aeson
import Data.Text

type Request = WebSocketRequestWrapper NormalRequest PushRequest
  -- Normal requests
type NormalRequest = (Request1 :<|> Request2 :<|> RequestCommon)
  -- Push based requests
type PushRequest = (Request4 :<|> Request5 :<|> RequestCommon)

data Request1 = Request1 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Request2 = Request2 (Text, Text)
  deriving (Generic, Show, ToJSON, FromJSON)
data RequestCommon = RequestCommon [Text]
  deriving (Generic, Show, ToJSON, FromJSON)

data Request4 = Request4 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Request5 = Request5 (Text, Text)
  deriving (Generic, Show, ToJSON, FromJSON)

data Response1 = Response1 Int
  deriving (Generic, Show, ToJSON, FromJSON)
data Response2 = Response2 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Response3 = Response3 (Text, Int)
  deriving (Generic, Show, ToJSON, FromJSON)

data Response4 = Response4 Int
  deriving (Generic, Show, ToJSON, FromJSON)
data Response5 = Response5 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Response6 = Response (Text, Int)
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage NormalRequest Request1 where
  type ResponseT NormalRequest Request1 = Response1

instance WebSocketMessage NormalRequest Request2 where
  type ResponseT NormalRequest Request2 = Response2

instance WebSocketMessage NormalRequest RequestCommon where
  type ResponseT NormalRequest RequestCommon = Response3

instance WebSocketMessage PushRequest Request4 where
  type ResponseT PushRequest Request4 = Response4

instance WebSocketMessage PushRequest Request5 where
  type ResponseT PushRequest Request5 = Response5

instance WebSocketMessage PushRequest RequestCommon where
  type ResponseT PushRequest RequestCommon = Response6
