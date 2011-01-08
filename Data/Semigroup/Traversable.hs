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
-- import Data.Functor.Identity

class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: FunctorApply f => (a -> f b) -> t a -> f (t b)
  sequence1 :: FunctorApply f => t (f b) -> f (t b)

foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)

{-
instance Traversable1 Identity where
  traverse1 f (Identity a) = Identity <$> f a
-}


{-
instance Traversable ((,)a) where traverse = traverse1
instance Traversable1 ((,)a) where 
  traverse1 f (a,b) = (,) a <$> f b

instance Traversable ((,,) a b) where traverse = traverse1
instance Traversable1 ((,) a b) where 
  traverse1 f (a,b,c) = (,) a b <$> f c

instance Traversable ((,,) a b c) where traverse = traverse1
instance Traversable1 ((,,) a b c) where 
  traverse1 f (a,b,c,d) = (,,) a b c <$> f d

instance Traversable ((,,,) a b c d) where traverse = traverse1
instance Traversable1 ((,,,) a b c d) where 
  traverse1 f (a,b,c,d,e) = (,,,) a b c d <$> f e
-}
