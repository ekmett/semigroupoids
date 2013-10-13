{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Groupoid
  ( Groupoid(..)
  ) where

import Data.Semigroupoid
import Data.Semigroupoid.Dual

-- | semigroupoid with inverses. This technically should be a category with inverses, except we need to use Ob to define the valid objects for the category
class Semigroupoid k => Groupoid k where
  inv :: k a b -> k b a

instance Groupoid k => Groupoid (Dual k) where
  inv (Dual k) = Dual (inv k)
