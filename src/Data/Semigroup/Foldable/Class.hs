{-# LANGUAGE CPP, TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 702
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
module Data.Semigroup.Foldable.Class
  ( Foldable1(..)
  , Bifoldable1(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Identity
import Data.Bifoldable
import Data.Bifunctor.Biff
import Data.Bifunctor.Clown
import Data.Bifunctor.Flip
import Data.Bifunctor.Join
import Data.Bifunctor.Product as Bifunctor
import Data.Bifunctor.Joker
import Data.Bifunctor.Tannen
import Data.Bifunctor.Wrapped
import Data.Foldable

import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Functor.Sum as Functor
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty(..))

#if MIN_VERSION_base(4,4,0)
import Data.Complex
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

import Data.Traversable.Instances ()

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup hiding (Product, Sum)
import Data.Orphans ()
-- import Data.Ord -- missing Foldable, https://ghc.haskell.org/trac/ghc/ticket/15098#ticket

#ifdef MIN_VERSION_generic_deriving
import Generics.Deriving.Base
#else
import GHC.Generics
#endif

import Prelude hiding (foldr)

class Foldable t => Foldable1 t where
  fold1 :: Semigroup m => t m -> m
  foldMap1 :: Semigroup m => (a -> m) -> t a -> m
  toNonEmpty :: t a -> NonEmpty a

  foldMap1 f = maybe (error "foldMap1") id . getOption . foldMap (Option . Just . f)
  fold1 = foldMap1 id
  toNonEmpty = foldMap1 (:|[])

instance Foldable1 Monoid.Sum where
  foldMap1 f (Monoid.Sum a) = f a

instance Foldable1 Monoid.Product where
  foldMap1 f (Monoid.Product a) = f a

instance Foldable1 Monoid.Dual where
  foldMap1 f (Monoid.Dual a) = f a

#if MIN_VERSION_base(4,8,0)
instance Foldable1 f => Foldable1 (Monoid.Alt f) where
  foldMap1 g (Monoid.Alt m) = foldMap1 g m
#endif

instance Foldable1 Semigroup.First where
  foldMap1 f (Semigroup.First a) = f a

instance Foldable1 Semigroup.Last where
  foldMap1 f (Semigroup.Last a) = f a

instance Foldable1 Semigroup.Min where
  foldMap1 f (Semigroup.Min a) = f a

instance Foldable1 Semigroup.Max where
  foldMap1 f (Semigroup.Max a) = f a

instance Foldable1 f => Foldable1 (Rec1 f) where
  foldMap1 f (Rec1 as) = foldMap1 f as

instance Foldable1 f => Foldable1 (M1 i c f) where
  foldMap1 f (M1 as) = foldMap1 f as

instance Foldable1 Par1 where
  foldMap1 f (Par1 a) = f a

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :*: g) where
  foldMap1 f (as :*: bs) = foldMap1 f as <> foldMap1 f bs

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
  foldMap1 f (L1 as) = foldMap1 f as
  foldMap1 f (R1 bs) = foldMap1 f bs

instance Foldable1 V1 where
  foldMap1 _ v = v `seq` undefined

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :.: g) where
  foldMap1 f (Comp1 m) = foldMap1 (foldMap1 f) m

class Bifoldable t => Bifoldable1 t where
  bifold1 :: Semigroup m => t m m -> m
  bifold1 = bifoldMap1 id id
  {-# INLINE bifold1 #-}

  bifoldMap1 :: Semigroup m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMap1 f g = maybe (error "bifoldMap1") id . getOption . bifoldMap (Option . Just . f) (Option . Just . g)
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 Arg where
  bifoldMap1 f g (Arg a b) = f a <> g b

instance Bifoldable1 Either where
  bifoldMap1 f _ (Left a) = f a
  bifoldMap1 _ g (Right b) = g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 (,) where
  bifoldMap1 f g (a, b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,) x) where
  bifoldMap1 f g (_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,) x y) where
  bifoldMap1 f g (_,_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,,) x y z) where
  bifoldMap1 f g (_,_,_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 Const where
  bifoldMap1 f _ (Const a) = f a
  {-# INLINE bifoldMap1 #-}

#ifdef MIN_VERSION_tagged
instance Bifoldable1 Tagged where
  bifoldMap1 _ g (Tagged b) = g b
  {-# INLINE bifoldMap1 #-}
#endif

instance (Bifoldable1 p, Foldable1 f, Foldable1 g) => Bifoldable1 (Biff p f g) where
  bifoldMap1 f g = bifoldMap1 (foldMap1 f) (foldMap1 g) . runBiff
  {-# INLINE bifoldMap1 #-}

instance Foldable1 f => Bifoldable1 (Clown f) where
  bifoldMap1 f _ = foldMap1 f . runClown
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 p => Bifoldable1 (Flip p) where
  bifoldMap1 f g = bifoldMap1 g f . runFlip
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 p => Foldable1 (Join p) where
  foldMap1 f (Join a) = bifoldMap1 f f a
  {-# INLINE foldMap1 #-}

instance Foldable1 g => Bifoldable1 (Joker g) where
  bifoldMap1 _ g = foldMap1 g . runJoker
  {-# INLINE bifoldMap1 #-}

instance (Bifoldable1 f, Bifoldable1 g) => Bifoldable1 (Bifunctor.Product f g) where
  bifoldMap1 f g (Bifunctor.Pair x y) = bifoldMap1 f g x <> bifoldMap1 f g y
  {-# INLINE bifoldMap1 #-}

instance (Foldable1 f, Bifoldable1 p) => Bifoldable1 (Tannen f p) where
  bifoldMap1 f g = foldMap1 (bifoldMap1 f g) . runTannen
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 p => Bifoldable1 (WrappedBifunctor p) where
  bifoldMap1 f g = bifoldMap1 f g . unwrapBifunctor
  {-# INLINE bifoldMap1 #-}

#if MIN_VERSION_base(4,4,0)
instance Foldable1 Complex where
  foldMap1 f (a :+ b) = f a <> f b
  {-# INLINE foldMap1 #-}
#endif

#ifdef MIN_VERSION_containers
instance Foldable1 Tree where
  foldMap1 f (Node a []) = f a
  foldMap1 f (Node a (x:xs)) = f a <> foldMap1 (foldMap1 f) (x :| xs)
#endif

instance Foldable1 Identity where
  foldMap1 f = f . runIdentity

#ifdef MIN_VERSION_tagged
instance Foldable1 (Tagged a) where
  foldMap1 f (Tagged a) = f a
#endif

instance Foldable1 m => Foldable1 (IdentityT m) where
  foldMap1 f = foldMap1 f . runIdentityT

instance Foldable1 f => Foldable1 (Backwards f) where
  foldMap1 f = foldMap1 f . forwards

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
  foldMap1 f = foldMap1 (foldMap1 f) . getCompose

instance Foldable1 f => Foldable1 (Lift f) where
  foldMap1 f (Pure x)  = f x
  foldMap1 f (Other y) = foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
  foldMap1 f (Functor.Pair a b) = foldMap1 f a <> foldMap1 f b

instance Foldable1 f => Foldable1 (Reverse f) where
  foldMap1 f = getDual . foldMap1 (Dual . f) . getReverse

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
  foldMap1 f (Functor.InL x) = foldMap1 f x
  foldMap1 f (Functor.InR y) = foldMap1 f y

instance Foldable1 NonEmpty where
  foldMap1 f (a :| []) = f a
  foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)
  toNonEmpty = id

instance Foldable1 ((,) a) where
  foldMap1 f (_, x) = f x

instance Foldable1 g => Foldable1 (Joker g a) where
  foldMap1 g = foldMap1 g . runJoker
  {-# INLINE foldMap1 #-}
