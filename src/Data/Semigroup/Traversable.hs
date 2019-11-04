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
module Data.Semigroup.Traversable
  ( NonEmptyTraversable(..)
  , foldMapNEDefault
  ) where

import Control.Applicative
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Traversable.Class

foldMapNEDefault :: (NonEmptyTraversable f, Semigroup m) => (a -> m) -> f a -> m
foldMapNEDefault f = getConst . traverseNE (Const . f)
