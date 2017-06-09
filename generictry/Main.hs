{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

import GHC.Generics
import Data.Aeson
import Data.Typeable (Typeable)

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

class (TypeNeq r a) => ToSumType r a where
  toSum :: a -> r

instance {-# OVERLAPPABLE #-} (TypeNeq a b, TypeNeq (a :<|> b) a) => ToSumType (a :<|> b) a where
  toSum a = Terminal a

instance {-# OVERLAPPABLE #-} (ToSumType b c, TypeNeq (a :<|> b) c) => ToSumType (a :<|> b) c where
  toSum c = Recurse (toSum c)

type MyRequest = Int :<|> Char :<|> Double :<|> ()

main = do
  let ab1 = [Recurse (2 ::Int) , Terminal "dd"]
  print (length ab1)
  print $ toJSON ab1
  print $ (toSum (4 :: Int) :: MyRequest)
  print $ (toSum ('c' :: Char) :: MyRequest)
  print $ (toSum (1.2 :: Double) :: MyRequest)
