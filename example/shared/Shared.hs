{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared where

import Reflex.Dom.WebSocket.Message

import GHC.Generics
import Data.Aeson
import Data.Text

type Request = Request1 :<|> Request2 :<|> Request3 :<|> Request4

data Request1 = Request1 Text
  deriving (Generic, Show)
data Request2 = Request2 (Text, Text)
  deriving (Generic, Show)
data Request3 = Request3 [Text]
  deriving (Generic, Show)
data Request4 = Request4 Text
  deriving (Generic, Show)

data Response1 = Response1 Int
  deriving (Generic, Show)
data Response2 = Response2 Text
  deriving (Generic, Show)
data Response3 = Response3 (Text, Int)
  deriving (Generic, Show)

instance WebSocketMessage Request Request1 where
  type ResponseT Request Request1 = Response1

instance WebSocketMessage Request Request2 where
  type ResponseT Request Request2 = Response2

instance WebSocketMessage Request Request3 where
  type ResponseT Request Request3 = Response3

instance WebSocketMessage Request Request4 where
  type ResponseT Request Request4 = Response1

instance ToJSON (Request1) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request1)
instance ToJSON (Request2) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request2)
instance ToJSON (Request3) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request3)
instance ToJSON (Request4) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Request4)

instance ToJSON (Response1) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Response1)

instance ToJSON (Response2) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Response2)
instance ToJSON (Response3) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (Response3)
