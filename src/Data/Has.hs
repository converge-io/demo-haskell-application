{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Data.Has where

import Prelude
import Data.Generics.Internal.VL.Lens as L
import Data.Generics.Product.Typed

-- Describes that a certain thing contains another thing
class Has a b where
  getter :: b -> a
  modifier :: (a -> a) -> b -> b

  default getter :: HasType a b => b -> a
  getter = getTyped @a

  default modifier :: HasType a b => (a -> a) -> b -> b
  modifier = L.over $ typed @a

instance Has a a where
  getter = id
  modifier = id
