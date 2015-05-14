{-# LANGUAGE PolyKinds, FlexibleInstances #-}

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

-- | semigroupoid with inverses. This technically should be a category with inverses, except we need to use Ob to define the valid objects for the category
class Semigroupoid k => Groupoid k where
  inv :: k a b -> k b a

instance Groupoid k => Groupoid (Dual k) where
  inv (Dual k) = Dual (inv k)
