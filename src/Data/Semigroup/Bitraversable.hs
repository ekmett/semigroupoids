{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Semigroup.Bitraversable
  ( Bitraversable1(..)
  , bifoldMap1Default
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Semigroup.Traversable.Class

bifoldMap1Default :: (Bitraversable1 t, Semigroup m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMap1Default f g = getConst . bitraverse1 (Const . f) (Const . g)
{-# INLINE bifoldMap1Default #-}
