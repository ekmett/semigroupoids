-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Bitraversable
-- Copyright   :  (C) 2011 Edward Kmett
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
import Data.Functor.Apply
import Data.Semigroup.Bifoldable
import Data.Bitraversable
import Data.Bifunctor
import Data.Semigroup

class (Bifoldable1 t, Bitraversable t) => Bitraversable1 t where
  bitraverse1 :: Apply f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
  bitraverse1 f g  = bisequence1 . bimap f g

  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 id id

bifoldMap1Default :: (Bitraversable1 t, Semigroup m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMap1Default f g = getConst . bitraverse1 (Const . f) (Const . g)

instance Bitraversable1 Either where
  bitraverse1 f _ (Left a) = Left <$> f a
  bitraverse1 _ g (Right b) = Right <$> g b

instance Bitraversable1 (,) where
  bitraverse1 f g (a, b) = (,) <$> f a <.> g b
