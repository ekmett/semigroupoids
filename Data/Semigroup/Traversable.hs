-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Traversable
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Semigroup.Traversable
  ( Traversable1(..)
  , foldMap1Default
  ) where

import Control.Applicative
import Data.Functor.Apply
import Data.Semigroup.Foldable
import Data.Traversable
import Data.Semigroup

class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  sequence1 :: Apply f => t (f b) -> f (t b)

  sequence1 = traverse1 id
  traverse1 f = sequence1 . fmap f

foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)
