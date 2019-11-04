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
module Data.Semigroup.Bifoldable
  ( NonEmptyBifoldable(..)
  , bitraverseNE_
  , bifor1_
  , bisequenceA1_
  , bifoldMapDefaultNE
  ) where

import Control.Applicative
import Data.Bifoldable
import Data.Functor.Semiapplicative
import Data.Semigroup
import Data.Semigroup.Foldable.Class
import Prelude hiding (foldr)

newtype Act f a = Act { getAct :: f a }

instance Semiapplicative f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)
  {-# INLINE (<>) #-}

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  {-# INLINE fmap #-}
  b <$ Act a = Act (b <$ a)
  {-# INLINE (<$) #-}

bitraverseNE_ :: (NonEmptyBifoldable t, Semiapplicative f) => (a -> f b) -> (c -> f d) -> t a c -> f ()
bitraverseNE_ f g t = getAct (bifoldMapNE (Act . ignore . f) (Act . ignore . g) t)
{-# INLINE bitraverseNE_ #-}

bifor1_ :: (NonEmptyBifoldable t, Semiapplicative f) => t a c -> (a -> f b) -> (c -> f d) -> f ()
bifor1_ t f g = bitraverseNE_ f g t
{-# INLINE bifor1_ #-}

ignore :: Functor f => f a -> f ()
ignore = (() <$)
{-# INLINE ignore #-}

bisequenceA1_ :: (NonEmptyBifoldable t, Semiapplicative f) => t (f a) (f b) -> f ()
bisequenceA1_ t = getAct (bifoldMapNE (Act . ignore) (Act . ignore) t)
{-# INLINE bisequenceA1_ #-}

-- | Usable default for foldMap, but only if you define bifoldMapNE yourself
bifoldMapDefaultNE :: (NonEmptyBifoldable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefaultNE f g = unwrapMonoid . bifoldMap (WrapMonoid . f) (WrapMonoid . g)
{-# INLINE bifoldMapDefaultNE #-}
