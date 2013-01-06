-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Foldable
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Semigroup.Foldable
  ( Foldable1(..)
  , traverse1_
  , for1_
  , sequenceA1_
  , foldMapDefault1
  ) where

import Control.Monad.Trans.Identity
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Functor.Product
import Data.Functor.Compose
import Data.Tree
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable.Instances ()
import Data.Semigroup hiding (Product)
import Prelude hiding (foldr)

class Foldable t => Foldable1 t where
  fold1 :: Semigroup m => t m -> m
  foldMap1 :: Semigroup m => (a -> m) -> t a -> m

  foldMap1 f = maybe (error "foldMap1") id . getOption . foldMap (Option . Just . f) 
  fold1 = foldMap1 id

instance Foldable1 Tree where
  foldMap1 f (Node a []) = f a
  foldMap1 f (Node a (x:xs)) = f a <> foldMap1 (foldMap1 f) (x :| xs)

instance Foldable1 Identity where
  foldMap1 f = f . runIdentity

instance Foldable1 m => Foldable1 (IdentityT m) where
  foldMap1 f = foldMap1 f . runIdentityT

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
  foldMap1 f = foldMap1 (foldMap1 f) . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
  foldMap1 f (Pair a b) = foldMap1 f a <> foldMap1 f b

instance Foldable1 NonEmpty where
  foldMap1 f (a :| []) = f a
  foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)

newtype Act f a = Act { getAct :: f a }

instance Apply f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  b <$ Act a = Act (b <$ a)

traverse1_ :: (Foldable1 t, Apply f) => (a -> f b) -> t a -> f ()
traverse1_ f t = () <$ getAct (foldMap1 (Act . f) t)
{-# INLINE traverse1_ #-}

for1_ :: (Foldable1 t, Apply f) => t a -> (a -> f b) -> f ()
for1_ = flip traverse1_
{-# INLINE for1_ #-}

sequenceA1_ :: (Foldable1 t, Apply f) => t (f a) -> f ()
sequenceA1_ t = () <$ getAct (foldMap1 Act t)
{-# INLINE sequenceA1_ #-}

-- | Usable default for foldMap, but only if you define foldMap1 yourself
foldMapDefault1 :: (Foldable1 t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault1 f = unwrapMonoid . foldMap (WrapMonoid . f)
{-# INLINE foldMapDefault1 #-}

-- toStream :: Foldable1 t => t a -> Stream a
-- concat1 :: Foldable1 t => t (Stream a) -> Stream a
-- concatMap1 :: Foldable1 t => (a -> Stream b) -> t a -> Stream b
