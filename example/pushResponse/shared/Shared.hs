{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module PushShared where

import Reflex.Dom.WebSocket.Message

import GHC.Generics
import Data.Aeson
import Data.Text

type Request = RequestWrapper
  -- Normal requests
  (Request1 :<|> Request2 :<|> Request3)
  -- Push based requests
  (Request4 :<|> Request5 :<|> Request6)

data Request1 = Request1 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Request2 = Request2 (Text, Text)
  deriving (Generic, Show, ToJSON, FromJSON)
data Request3 = Request3 [Text]
  deriving (Generic, Show, ToJSON, FromJSON)

data Request4 = Request4 Text
  deriving (Generic, Show, ToJSON, FromJSON)
data Request5 = Request5 (Text, Text)
  deriving (Generic, Show, ToJSON, FromJSON)
data Request6 = Request6 [Text]
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

instance WebSocketMessage Request Request1 where
  type ResponseT Request Request1 = Response1

instance WebSocketMessage Request Request2 where
  type ResponseT Request Request2 = Response2

instance WebSocketMessage Request Request3 where
  type ResponseT Request Request3 = Response3

instance WebSocketMessage Request Request4 where
  type ResponseT Request Request4 = Response4

instance WebSocketMessage Request Request5 where
  type ResponseT Request Request5 = Response5

instance WebSocketMessage Request Request6 where
  type ResponseT Request Request6 = Response6
