{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Generics
import Data.Aeson

data True
data False

type family TypeEqF a b where
  TypeEqF a a = True
  TypeEqF a b = False

type TypeNeq a b = TypeEqF a b ~ False

data AorB a b = A a | B b
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (AorB a b)

class (TypeNeq r a) => WebSockerRequest r a where
  toSum :: a -> r

instance {-# OVERLAPPABLE #-} (TypeNeq a b, TypeNeq (AorB a b) b) => WebSockerRequest (AorB a b) b where
  toSum b = B b

instance {-# OVERLAPPABLE #-} (WebSockerRequest a c, TypeNeq (AorB a b) c) => WebSockerRequest (AorB a b) c where
  toSum c = A (toSum c)

main = do
  let ab1 = [A (2 ::Int) , B "dd"]
  print (length ab1)
  print $ toJSON ab1
