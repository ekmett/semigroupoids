{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  polykinds
--
----------------------------------------------------------------------------

module Data.Groupoid
  ( Groupoid(..)
  ) where

import Data.Semigroupoid
import Data.Semigroupoid.Dual

#if MIN_VERSION_base(4,7,0)
import qualified Data.Type.Coercion as Co
import qualified Data.Type.Equality as Eq
#endif

-- | semigroupoid with inverses. This technically should be a category with inverses, except we need to use Ob to define the valid objects for the category
class Semigroupoid k => Groupoid k where
  inv :: k a b -> k b a

instance Groupoid k => Groupoid (Dual k) where
  inv (Dual k) = Dual (inv k)

#if MIN_VERSION_base(4,7,0)
instance Groupoid Co.Coercion where
  inv = Co.sym

instance Groupoid (Eq.:~:) where
  inv = Eq.sym
#endif

#if MIN_VERSION_base(4,10,0)
instance Groupoid (Eq.:~~:) where
  inv Eq.HRefl = Eq.HRefl
#endif
