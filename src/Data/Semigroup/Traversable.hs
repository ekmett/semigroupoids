{-# LANGUAGE CPP #-}
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
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Identity
import Data.Functor.Apply
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup hiding (Product, Sum)
import Data.Semigroup.Foldable
import Data.Traversable
import Data.Traversable.Instances ()

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

#ifdef MIN_VERSION_comonad
import Data.Functor.Coproduct
#endif

class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  sequence1 :: Apply f => t (f b) -> f (t b)

  sequence1 = traverse1 id
  traverse1 f = sequence1 . fmap f

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL traverse1 | sequence1 #-}
#endif

foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)

instance Traversable1 Identity where
  traverse1 f = fmap Identity . f . runIdentity

instance Traversable1 f => Traversable1 (IdentityT f) where
  traverse1 f = fmap IdentityT . traverse1 f . runIdentityT

instance Traversable1 f => Traversable1 (Backwards f) where
  traverse1 f = fmap Backwards . traverse1 f . forwards

instance (Traversable1 f, Traversable1 g) => Traversable1 (Compose f g) where
  traverse1 f = fmap Compose . traverse1 (traverse1 f) . getCompose

instance Traversable1 f => Traversable1 (Lift f) where
  traverse1 f (Pure x)  = Pure <$> f x
  traverse1 f (Other y) = Other <$> traverse1 f y

instance (Traversable1 f, Traversable1 g) => Traversable1 (Product f g) where
  traverse1 f (Pair a b) = Pair <$> traverse1 f a <.> traverse1 f b

instance Traversable1 f => Traversable1 (Reverse f) where
  traverse1 f = fmap Reverse . forwards . traverse1 (Backwards . f) . getReverse

instance (Traversable1 f, Traversable1 g) => Traversable1 (Sum f g) where
  traverse1 f (InL x) = InL <$> traverse1 f x
  traverse1 f (InR y) = InR <$> traverse1 f y

#ifdef MIN_VERSION_comonad
instance (Traversable1 f, Traversable1 g) => Traversable1 (Coproduct f g) where
  traverse1 f = coproduct
    (fmap (Coproduct . Left) . traverse1 f)
    (fmap (Coproduct . Right) . traverse1 f)
#endif

#ifdef MIN_VERSION_containers
instance Traversable1 Tree where
  traverse1 f (Node a []) = (`Node`[]) <$> f a
  traverse1 f (Node a (x:xs)) = (\b (y:|ys) -> Node b (y:ys)) <$> f a <.> traverse1 (traverse1 f) (x :| xs)
#endif

instance Traversable1 NonEmpty where
  traverse1 f (a :| []) = (:|[]) <$> f a
  traverse1 f (a :| (b: bs)) = (\a' (b':| bs') -> a' :| b': bs') <$> f a <.> traverse1 f (b :| bs)

instance Traversable1 ((,) a) where
  traverse1 f (a, b) = (,) a <$> f b
