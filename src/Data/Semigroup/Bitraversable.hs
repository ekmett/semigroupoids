{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
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
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Traversable.Class

bifoldMap1Default :: (Bitraversable1 t, Semigroup m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMap1Default f g = getConst . bitraverse1 (Const . f) (Const . g)
{-# INLINE bifoldMap1Default #-}
