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
module Data.Semigroup.Traversable.Class
  ( NonEmptyBitraversable(..)
  , NonEmptyTraversable(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Identity
import Data.Bitraversable
import Data.Bifunctor
import Data.Bifunctor.Biff
import Data.Bifunctor.Clown
import Data.Bifunctor.Flip
import Data.Bifunctor.Joker
import Data.Bifunctor.Join
import Data.Bifunctor.Product as Bifunctor
import Data.Bifunctor.Tannen
import Data.Bifunctor.Wrapped
import Data.Functor.Semiapplicative
import Data.Functor.Compose

import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Functor.Sum as Functor
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Orphans ()
import Data.Semigroup as Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Bifoldable
#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import Data.Traversable.Instances ()

#if MIN_VERSION_base(4,4,0)
import Data.Complex
#endif

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

#ifdef MIN_VERSION_generic_deriving
import Generics.Deriving.Base
#else
import GHC.Generics
#endif

class (NonEmptyBifoldable t, Bitraversable t) => NonEmptyBitraversable t where
  bitraverseNE :: Semiapplicative f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
  bitraverseNE f g  = bisequenceNE . bimap f g
  {-# INLINE bitraverseNE #-}

  bisequenceNE :: Semiapplicative f => t (f a) (f b) -> f (t a b)
  bisequenceNE = bitraverseNE id id
  {-# INLINE bisequenceNE #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bitraverseNE | bisequenceNE #-}
#endif

instance NonEmptyBitraversable Arg where
  bitraverseNE f g (Arg a b) = Arg <$> f a <.> g b

instance NonEmptyBitraversable Either where
  bitraverseNE f _ (Left a) = Left <$> f a
  bitraverseNE _ g (Right b) = Right <$> g b
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable (,) where
  bitraverseNE f g (a, b) = (,) <$> f a <.> g b
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable ((,,) x) where
  bitraverseNE f g (x, a, b) = (,,) x <$> f a <.> g b
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable ((,,,) x y) where
  bitraverseNE f g (x, y, a, b) = (,,,) x y <$> f a <.> g b
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable ((,,,,) x y z) where
  bitraverseNE f g (x, y, z, a, b) = (,,,,) x y z <$> f a <.> g b
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable Const where
  bitraverseNE f _ (Const a) = Const <$> f a
  {-# INLINE bitraverseNE #-}

#ifdef MIN_VERSION_tagged
instance NonEmptyBitraversable Tagged where
  bitraverseNE _ g (Tagged b) = Tagged <$> g b
  {-# INLINE bitraverseNE #-}
#endif

instance (NonEmptyBitraversable p, NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyBitraversable (Biff p f g) where
  bitraverseNE f g = fmap Biff . bitraverseNE (traverseNE f) (traverseNE g) . runBiff
  {-# INLINE bitraverseNE #-}

instance NonEmptyTraversable f => NonEmptyBitraversable (Clown f) where
  bitraverseNE f _ = fmap Clown . traverseNE f . runClown
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable p => NonEmptyBitraversable (Flip p) where
  bitraverseNE f g = fmap Flip . bitraverseNE g f . runFlip
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable p => NonEmptyTraversable (Join p) where
  traverseNE f (Join a) = fmap Join (bitraverseNE f f a)
  {-# INLINE traverseNE #-}
  sequenceNE (Join a) = fmap Join (bisequenceNE a)
  {-# INLINE sequenceNE #-}

instance NonEmptyTraversable g => NonEmptyBitraversable (Joker g) where
  bitraverseNE _ g = fmap Joker . traverseNE g . runJoker
  {-# INLINE bitraverseNE #-}

instance (NonEmptyBitraversable f, NonEmptyBitraversable g) => NonEmptyBitraversable (Bifunctor.Product f g) where
  bitraverseNE f g (Bifunctor.Pair x y) = Bifunctor.Pair <$> bitraverseNE f g x <.> bitraverseNE f g y
  {-# INLINE bitraverseNE #-}

instance (NonEmptyTraversable f, NonEmptyBitraversable p) => NonEmptyBitraversable (Tannen f p) where
  bitraverseNE f g = fmap Tannen . traverseNE (bitraverseNE f g) . runTannen
  {-# INLINE bitraverseNE #-}

instance NonEmptyBitraversable p => NonEmptyBitraversable (WrappedBifunctor p) where
  bitraverseNE f g = fmap WrapBifunctor . bitraverseNE f g . unwrapBifunctor
  {-# INLINE bitraverseNE #-}


class (NonEmptyFoldable t, Traversable t) => NonEmptyTraversable t where
  traverseNE :: Semiapplicative f => (a -> f b) -> t a -> f (t b)
  sequenceNE :: Semiapplicative f => t (f b) -> f (t b)

  sequenceNE = traverseNE id
  traverseNE f = sequenceNE . fmap f

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL traverseNE | sequenceNE #-}
#endif

instance NonEmptyTraversable f => NonEmptyTraversable (Rec1 f) where
  traverseNE f (Rec1 as) = Rec1 <$> traverseNE f as

instance NonEmptyTraversable f => NonEmptyTraversable (M1 i c f) where
  traverseNE f (M1 as) = M1 <$> traverseNE f as

instance NonEmptyTraversable Par1 where
  traverseNE f (Par1 a) = Par1 <$> f a

instance NonEmptyTraversable V1 where
  traverseNE _ v = v `seq` undefined

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (f :*: g) where
  traverseNE f (as :*: bs) = (:*:) <$> traverseNE f as <.> traverseNE f bs

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (f :+: g) where
  traverseNE f (L1 as) = L1 <$> traverseNE f as
  traverseNE f (R1 bs) = R1 <$> traverseNE f bs

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (f :.: g) where
  traverseNE f (Comp1 m) = Comp1 <$> traverseNE (traverseNE f) m

instance NonEmptyTraversable Identity where
  traverseNE f = fmap Identity . f . runIdentity

instance NonEmptyTraversable f => NonEmptyTraversable (IdentityT f) where
  traverseNE f = fmap IdentityT . traverseNE f . runIdentityT

instance NonEmptyTraversable f => NonEmptyTraversable (Backwards f) where
  traverseNE f = fmap Backwards . traverseNE f . forwards

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (Compose f g) where
  traverseNE f = fmap Compose . traverseNE (traverseNE f) . getCompose

instance NonEmptyTraversable f => NonEmptyTraversable (Lift f) where
  traverseNE f (Pure x)  = Pure <$> f x
  traverseNE f (Other y) = Other <$> traverseNE f y

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (Functor.Product f g) where
  traverseNE f (Functor.Pair a b) = Functor.Pair <$> traverseNE f a <.> traverseNE f b

instance NonEmptyTraversable f => NonEmptyTraversable (Reverse f) where
  traverseNE f = fmap Reverse . forwards . traverseNE (Backwards . f) . getReverse

instance (NonEmptyTraversable f, NonEmptyTraversable g) => NonEmptyTraversable (Functor.Sum f g) where
  traverseNE f (Functor.InL x) = Functor.InL <$> traverseNE f x
  traverseNE f (Functor.InR y) = Functor.InR <$> traverseNE f y

#if MIN_VERSION_base(4,4,0)
instance NonEmptyTraversable Complex where
  traverseNE f (a :+ b) = (:+) <$> f a <.> f b
  {-# INLINE traverseNE #-}
#endif

#ifdef MIN_VERSION_tagged
instance NonEmptyTraversable (Tagged a) where
  traverseNE f (Tagged a) = Tagged <$> f a
#endif

#ifdef MIN_VERSION_containers
instance NonEmptyTraversable Tree where
  traverseNE f (Node a []) = (`Node`[]) <$> f a
  traverseNE f (Node a (x:xs)) = (\b (y:|ys) -> Node b (y:ys)) <$> f a <.> traverseNE (traverseNE f) (x :| xs)
#endif

instance NonEmptyTraversable NonEmpty where
  traverseNE f (a :| []) = (:|[]) <$> f a
  traverseNE f (a :| (b: bs)) = (\a' (b':| bs') -> a' :| b': bs') <$> f a <.> traverseNE f (b :| bs)

instance NonEmptyTraversable ((,) a) where
  traverseNE f (a, b) = (,) a <$> f b

instance NonEmptyTraversable g => NonEmptyTraversable (Joker g a) where
  traverseNE g = fmap Joker . traverseNE g . runJoker
  {-# INLINE traverseNE #-}

instance NonEmptyTraversable Monoid.Sum where
  traverseNE g (Monoid.Sum a) = Monoid.Sum <$> g a

instance NonEmptyTraversable Monoid.Product where
  traverseNE g (Monoid.Product a) = Monoid.Product <$> g a

instance NonEmptyTraversable Monoid.Dual where
  traverseNE g (Monoid.Dual a) = Monoid.Dual <$> g a

#if MIN_VERSION_base(4,8,0)
instance NonEmptyTraversable f => NonEmptyTraversable (Monoid.Alt f) where
  traverseNE g (Monoid.Alt m) = Monoid.Alt <$> traverseNE g m
#endif

instance NonEmptyTraversable Semigroup.First where
  traverseNE g (Semigroup.First a) = Semigroup.First <$> g a

instance NonEmptyTraversable Semigroup.Last where
  traverseNE g (Semigroup.Last a) = Semigroup.Last <$> g a

instance NonEmptyTraversable Semigroup.Min where
  traverseNE g (Semigroup.Min a) = Semigroup.Min <$> g a

instance NonEmptyTraversable Semigroup.Max where
  traverseNE g (Semigroup.Max a) = Semigroup.Max <$> g a
