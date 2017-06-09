{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- instance (TypeNeq a b) => WebSockerRequest (AorB a b) a where
--   toSum a = A a

instance (TypeNeq () b, TypeNeq (AorB () b) b) => WebSockerRequest (AorB () b) b where
  toSum b = B b

-- instance (TypeNeq a b, WebSockerRequest (AorB () b) b) => WebSockerRequest (AorB a (AorB () b)) b where
--   toSum b = B (toSum b)

-- instance (TypeNeq (AorB () b1) b2, TypeNeq b1 b2, TypeNeq (AorB (AorB () b1) b2) b2) => WebSockerRequest (AorB (AorB () b1) b2) b2 where
--   toSum b2 = B b2
instance (TypeNeq b1 b2, TypeNeq (AorB b1 b2) b2) => WebSockerRequest (AorB b1 b2) b2 where
  toSum b2 = B b2

-- instance (TypeNeq a b, TypeNeq (AorB a c) b, WebSockerRequest c b, TypeNeq (AorB () b) c, TypeNeq a ()) => WebSockerRequest (AorB a c) b where
--   toSum b = B (toSum b :: c)

-- instance (TypeNeq a b, TypeNeq b c, WebSockerRequest (AorB b c) c) => WebSockerRequest (AorB a (AorB b c)) c where
--   toSum c = B (toSum c)

-- instance (TypeNeq a b) => WebSockerRequest (AorB a (AorB b c)) b where
--   toSum b = B b

main = do
  let ab1 = [A (2 ::Int) , B "dd"]
  print (length ab1)
  print $ toJSON ab1
