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
module Data.Semigroup.Bifoldable
  ( Bifoldable1(..)
  , bitraverse1_
  , bifor1_
  , bisequenceA1_
  , bifoldMapDefault1
  ) where

import Control.Applicative
import Data.Bifoldable
import Data.Functor.Apply
import Data.Semigroup
import Data.Semigroup.Foldable.Class
import Prelude hiding (foldr)

newtype Act f a = Act { getAct :: f a }

instance Apply f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)
  {-# INLINE (<>) #-}

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  {-# INLINE fmap #-}
  b <$ Act a = Act (b <$ a)
  {-# INLINE (<$) #-}

bitraverse1_ :: (Bifoldable1 t, Apply f) => (a -> f b) -> (c -> f d) -> t a c -> f ()
bitraverse1_ f g t = getAct (bifoldMap1 (Act . ignore . f) (Act . ignore . g) t)
{-# INLINE bitraverse1_ #-}

bifor1_ :: (Bifoldable1 t, Apply f) => t a c -> (a -> f b) -> (c -> f d) -> f ()
bifor1_ t f g = bitraverse1_ f g t
{-# INLINE bifor1_ #-}

ignore :: Functor f => f a -> f ()
ignore = (() <$)
{-# INLINE ignore #-}

bisequenceA1_ :: (Bifoldable1 t, Apply f) => t (f a) (f b) -> f ()
bisequenceA1_ t = getAct (bifoldMap1 (Act . ignore) (Act . ignore) t)
{-# INLINE bisequenceA1_ #-}

-- | Usable default for foldMap, but only if you define bifoldMap1 yourself
bifoldMapDefault1 :: (Bifoldable1 t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault1 f g = unwrapMonoid . bifoldMap (WrapMonoid . f) (WrapMonoid . g)
{-# INLINE bifoldMapDefault1 #-}
