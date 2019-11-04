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
  ( NonEmptyFoldable(..)
  , NonEmptyBifoldable(..)
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

class Foldable t => NonEmptyFoldable t where
  foldNE :: Semigroup m => t m -> m
  foldMapNE :: Semigroup m => (a -> m) -> t a -> m
  toNonEmpty :: t a -> NonEmpty a

  foldMapNE f = maybe (error "foldMapNE") id . getOption . foldMap (Option . Just . f)
  foldNE = foldMapNE id
  toNonEmpty = foldMapNE (:|[])

instance NonEmptyFoldable Monoid.Sum where
  foldMapNE f (Monoid.Sum a) = f a

instance NonEmptyFoldable Monoid.Product where
  foldMapNE f (Monoid.Product a) = f a

instance NonEmptyFoldable Monoid.Dual where
  foldMapNE f (Monoid.Dual a) = f a

#if MIN_VERSION_base(4,8,0)
instance NonEmptyFoldable f => NonEmptyFoldable (Monoid.Alt f) where
  foldMapNE g (Monoid.Alt m) = foldMapNE g m
#endif

instance NonEmptyFoldable Semigroup.First where
  foldMapNE f (Semigroup.First a) = f a

instance NonEmptyFoldable Semigroup.Last where
  foldMapNE f (Semigroup.Last a) = f a

instance NonEmptyFoldable Semigroup.Min where
  foldMapNE f (Semigroup.Min a) = f a

instance NonEmptyFoldable Semigroup.Max where
  foldMapNE f (Semigroup.Max a) = f a

instance NonEmptyFoldable f => NonEmptyFoldable (Rec1 f) where
  foldMapNE f (Rec1 as) = foldMapNE f as

instance NonEmptyFoldable f => NonEmptyFoldable (M1 i c f) where
  foldMapNE f (M1 as) = foldMapNE f as

instance NonEmptyFoldable Par1 where
  foldMapNE f (Par1 a) = f a

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (f :*: g) where
  foldMapNE f (as :*: bs) = foldMapNE f as <> foldMapNE f bs

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (f :+: g) where
  foldMapNE f (L1 as) = foldMapNE f as
  foldMapNE f (R1 bs) = foldMapNE f bs

instance NonEmptyFoldable V1 where
  foldMapNE _ v = v `seq` undefined

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (f :.: g) where
  foldMapNE f (Comp1 m) = foldMapNE (foldMapNE f) m

class Bifoldable t => NonEmptyBifoldable t where
  bifoldNE :: Semigroup m => t m m -> m
  bifoldNE = bifoldMapNE id id
  {-# INLINE bifoldNE #-}

  bifoldMapNE :: Semigroup m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMapNE f g = maybe (error "bifoldMapNE") id . getOption . bifoldMap (Option . Just . f) (Option . Just . g)
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable Arg where
  bifoldMapNE f g (Arg a b) = f a <> g b

instance NonEmptyBifoldable Either where
  bifoldMapNE f _ (Left a) = f a
  bifoldMapNE _ g (Right b) = g b
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable (,) where
  bifoldMapNE f g (a, b) = f a <> g b
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable ((,,) x) where
  bifoldMapNE f g (_,a,b) = f a <> g b
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable ((,,,) x y) where
  bifoldMapNE f g (_,_,a,b) = f a <> g b
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable ((,,,,) x y z) where
  bifoldMapNE f g (_,_,_,a,b) = f a <> g b
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable Const where
  bifoldMapNE f _ (Const a) = f a
  {-# INLINE bifoldMapNE #-}

#ifdef MIN_VERSION_tagged
instance NonEmptyBifoldable Tagged where
  bifoldMapNE _ g (Tagged b) = g b
  {-# INLINE bifoldMapNE #-}
#endif

instance (NonEmptyBifoldable p, NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyBifoldable (Biff p f g) where
  bifoldMapNE f g = bifoldMapNE (foldMapNE f) (foldMapNE g) . runBiff
  {-# INLINE bifoldMapNE #-}

instance NonEmptyFoldable f => NonEmptyBifoldable (Clown f) where
  bifoldMapNE f _ = foldMapNE f . runClown
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable p => NonEmptyBifoldable (Flip p) where
  bifoldMapNE f g = bifoldMapNE g f . runFlip
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable p => NonEmptyFoldable (Join p) where
  foldMapNE f (Join a) = bifoldMapNE f f a
  {-# INLINE foldMapNE #-}

instance NonEmptyFoldable g => NonEmptyBifoldable (Joker g) where
  bifoldMapNE _ g = foldMapNE g . runJoker
  {-# INLINE bifoldMapNE #-}

instance (NonEmptyBifoldable f, NonEmptyBifoldable g) => NonEmptyBifoldable (Bifunctor.Product f g) where
  bifoldMapNE f g (Bifunctor.Pair x y) = bifoldMapNE f g x <> bifoldMapNE f g y
  {-# INLINE bifoldMapNE #-}

instance (NonEmptyFoldable f, NonEmptyBifoldable p) => NonEmptyBifoldable (Tannen f p) where
  bifoldMapNE f g = foldMapNE (bifoldMapNE f g) . runTannen
  {-# INLINE bifoldMapNE #-}

instance NonEmptyBifoldable p => NonEmptyBifoldable (WrappedBifunctor p) where
  bifoldMapNE f g = bifoldMapNE f g . unwrapBifunctor
  {-# INLINE bifoldMapNE #-}

#if MIN_VERSION_base(4,4,0)
instance NonEmptyFoldable Complex where
  foldMapNE f (a :+ b) = f a <> f b
  {-# INLINE foldMapNE #-}
#endif

#ifdef MIN_VERSION_containers
instance NonEmptyFoldable Tree where
  foldMapNE f (Node a []) = f a
  foldMapNE f (Node a (x:xs)) = f a <> foldMapNE (foldMapNE f) (x :| xs)
#endif

instance NonEmptyFoldable Identity where
  foldMapNE f = f . runIdentity

#ifdef MIN_VERSION_tagged
instance NonEmptyFoldable (Tagged a) where
  foldMapNE f (Tagged a) = f a
#endif

instance NonEmptyFoldable m => NonEmptyFoldable (IdentityT m) where
  foldMapNE f = foldMapNE f . runIdentityT

instance NonEmptyFoldable f => NonEmptyFoldable (Backwards f) where
  foldMapNE f = foldMapNE f . forwards

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (Compose f g) where
  foldMapNE f = foldMapNE (foldMapNE f) . getCompose

instance NonEmptyFoldable f => NonEmptyFoldable (Lift f) where
  foldMapNE f (Pure x)  = f x
  foldMapNE f (Other y) = foldMapNE f y

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (Functor.Product f g) where
  foldMapNE f (Functor.Pair a b) = foldMapNE f a <> foldMapNE f b

instance NonEmptyFoldable f => NonEmptyFoldable (Reverse f) where
  foldMapNE f = getDual . foldMapNE (Dual . f) . getReverse

instance (NonEmptyFoldable f, NonEmptyFoldable g) => NonEmptyFoldable (Functor.Sum f g) where
  foldMapNE f (Functor.InL x) = foldMapNE f x
  foldMapNE f (Functor.InR y) = foldMapNE f y

instance NonEmptyFoldable NonEmpty where
  foldMapNE f (a :| []) = f a
  foldMapNE f (a :| b : bs) = f a <> foldMapNE f (b :| bs)
  toNonEmpty = id

instance NonEmptyFoldable ((,) a) where
  foldMapNE f (_, x) = f x

instance NonEmptyFoldable g => NonEmptyFoldable (Joker g a) where
  foldMapNE g = foldMapNE g . runJoker
  {-# INLINE foldMapNE #-}
