{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
  ( Traversable1(..)
  -- * Defining Traversable1 instances
  -- $traversable1instances
  , traverse1Maybe
  -- * Default superclass instance helpers
  , foldMap1Default
  ) where

import Control.Applicative
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Traversable.Class
import Data.Functor.Bind.Class

-- | Default implementation of 'foldMap1' given an implementation of 'Traversable1'.
foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)

-- $traversable1instances
-- Defining 'Traversable1' instances for types with both 'Traversable1' and 'Traversable' 
-- substructures can be done with 'traverse1Maybe', '(<*.>)', and '(<.*>)'.
--
-- > data Foo a = Foo (Maybe a) (Maybe a) a [a]
-- >   deriving (Functor, Traversable, Foldable)
-- > instance Traversable1 Foo where
-- >   traverse1 f (Foo ma ma' a as) = Foo <$> traverseMaybe ma <*> traverseMaybe ma' <*.> f a <.*> traverseMaybe as
-- > instance Foldable1 Foo where
-- >   foldMap1 = foldMap1Default

