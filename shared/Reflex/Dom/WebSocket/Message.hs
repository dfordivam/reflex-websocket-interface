{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Reflex.Dom.WebSocket.Message
  (WebSocketMessage (..)
  , (:<|>)(..)
  , toSum
  , fromSum)
  where

import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.Typeable (Typeable)

-- The websocket is identified by sum type
-- Sum Type
-- type MyWebSocketRequest = Int :<|> Char :<|> Double :<|> ()
class ( ToJSON sum
      , FromJSON sum
      , ToJSON (ResponseT sum req)
      , FromJSON (ResponseT sum req)
      , Show req
      , Show (ResponseT sum req)
      , ToSumType sum req
      ) =>
      WebSocketMessage sum req where
  type ResponseT sum req

data True
data False

type family TypeEqF a b where
  TypeEqF a a = True
  TypeEqF a b = False

type TypeNeq a b = TypeEqF a b ~ False

data a :<|> b = Terminal a | Recurse b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Generic)
infixr 3 :<|>

instance (ToJSON a, ToJSON b) => ToJSON (a :<|> b)
instance (FromJSON a, FromJSON b) => FromJSON (a :<|> b)

class ToSumType r a where
  toSum :: a -> r
  fromSum :: r -> Maybe a

instance {-# OVERLAPPABLE #-} (TypeNeq a b, TypeNeq (a :<|> b) a) => ToSumType (a :<|> b) a where
  toSum a = Terminal a
  fromSum (Terminal a) = Just a
  fromSum (Recurse _) = Nothing

instance ToSumType a a where
  toSum a = a
  fromSum a = Just a

instance {-# OVERLAPPABLE #-} (ToSumType b c, TypeNeq (a :<|> b) c) => ToSumType (a :<|> b) c where
  toSum c = Recurse (toSum c)
  fromSum (Recurse b) = fromSum b
  fromSum (Terminal _) = Nothing
