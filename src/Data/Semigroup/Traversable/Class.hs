{-# LANGUAGE CPP, TypeOperators #-}
{-# LANGUAGE Trustworthy #-}


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
  ( Bitraversable1(..)
  , Traversable1(..)
  ) where

import Control.Applicative
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
import Data.Functor.Apply
import Data.Functor.Compose

import Data.Complex
import Data.Functor.Identity
import Data.Functor.Product as Functor
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
import Data.Traversable.Instances ()
import GHC.Generics

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Identity
import Data.Functor.Reverse

class (Bifoldable1 t, Bitraversable t) => Bitraversable1 t where
  bitraverse1 :: Apply f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
  bitraverse1 f g  = bisequence1 . bimap f g
  {-# INLINE bitraverse1 #-}

  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 id id
  {-# INLINE bisequence1 #-}

  {-# MINIMAL bitraverse1 | bisequence1 #-}

instance Bitraversable1 Arg where
  bitraverse1 f g (Arg a b) = Arg <$> f a <.> g b

instance Bitraversable1 Either where
  bitraverse1 f _ (Left a) = Left <$> f a
  bitraverse1 _ g (Right b) = Right <$> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 (,) where
  bitraverse1 f g (a, b) = (,) <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,) x) where
  bitraverse1 f g (x, a, b) = (,,) x <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,,) x y) where
  bitraverse1 f g (x, y, a, b) = (,,,) x y <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,,,) x y z) where
  bitraverse1 f g (x, y, z, a, b) = (,,,,) x y z <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 Const where
  bitraverse1 f _ (Const a) = Const <$> f a
  {-# INLINE bitraverse1 #-}

#ifdef MIN_VERSION_tagged
instance Bitraversable1 Tagged where
  bitraverse1 _ g (Tagged b) = Tagged <$> g b
  {-# INLINE bitraverse1 #-}
#endif

instance (Bitraversable1 p, Traversable1 f, Traversable1 g) => Bitraversable1 (Biff p f g) where
  bitraverse1 f g = fmap Biff . bitraverse1 (traverse1 f) (traverse1 g) . runBiff
  {-# INLINE bitraverse1 #-}

instance Traversable1 f => Bitraversable1 (Clown f) where
  bitraverse1 f _ = fmap Clown . traverse1 f . runClown
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 p => Bitraversable1 (Flip p) where
  bitraverse1 f g = fmap Flip . bitraverse1 g f . runFlip
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 p => Traversable1 (Join p) where
  traverse1 f (Join a) = fmap Join (bitraverse1 f f a)
  {-# INLINE traverse1 #-}
  sequence1 (Join a) = fmap Join (bisequence1 a)
  {-# INLINE sequence1 #-}

instance Traversable1 g => Bitraversable1 (Joker g) where
  bitraverse1 _ g = fmap Joker . traverse1 g . runJoker
  {-# INLINE bitraverse1 #-}

instance (Bitraversable1 f, Bitraversable1 g) => Bitraversable1 (Bifunctor.Product f g) where
  bitraverse1 f g (Bifunctor.Pair x y) = Bifunctor.Pair <$> bitraverse1 f g x <.> bitraverse1 f g y
  {-# INLINE bitraverse1 #-}

instance (Traversable1 f, Bitraversable1 p) => Bitraversable1 (Tannen f p) where
  bitraverse1 f g = fmap Tannen . traverse1 (bitraverse1 f g) . runTannen
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 p => Bitraversable1 (WrappedBifunctor p) where
  bitraverse1 f g = fmap WrapBifunctor . bitraverse1 f g . unwrapBifunctor
  {-# INLINE bitraverse1 #-}


class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  sequence1 :: Apply f => t (f b) -> f (t b)

  sequence1 = traverse1 id
  traverse1 f = sequence1 . fmap f

  {-# MINIMAL traverse1 | sequence1 #-}

instance Traversable1 f => Traversable1 (Rec1 f) where
  traverse1 f (Rec1 as) = Rec1 <$> traverse1 f as

instance Traversable1 f => Traversable1 (M1 i c f) where
  traverse1 f (M1 as) = M1 <$> traverse1 f as

instance Traversable1 Par1 where
  traverse1 f (Par1 a) = Par1 <$> f a

instance Traversable1 V1 where
  traverse1 _ v = v `seq` undefined

instance (Traversable1 f, Traversable1 g) => Traversable1 (f :*: g) where
  traverse1 f (as :*: bs) = (:*:) <$> traverse1 f as <.> traverse1 f bs

instance (Traversable1 f, Traversable1 g) => Traversable1 (f :+: g) where
  traverse1 f (L1 as) = L1 <$> traverse1 f as
  traverse1 f (R1 bs) = R1 <$> traverse1 f bs

instance (Traversable1 f, Traversable1 g) => Traversable1 (f :.: g) where
  traverse1 f (Comp1 m) = Comp1 <$> traverse1 (traverse1 f) m

instance Traversable1 Identity where
  traverse1 f = fmap Identity . f . runIdentity

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Product f g) where
  traverse1 f (Functor.Pair a b) = Functor.Pair <$> traverse1 f a <.> traverse1 f b

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Sum f g) where
  traverse1 f (Functor.InL x) = Functor.InL <$> traverse1 f x
  traverse1 f (Functor.InR y) = Functor.InR <$> traverse1 f y

instance (Traversable1 f, Traversable1 g) => Traversable1 (Compose f g) where
  traverse1 f = fmap Compose . traverse1 (traverse1 f) . getCompose

instance Traversable1 f => Traversable1 (IdentityT f) where
  traverse1 f = fmap IdentityT . traverse1 f . runIdentityT

instance Traversable1 f => Traversable1 (Backwards f) where
  traverse1 f = fmap Backwards . traverse1 f . forwards

instance Traversable1 f => Traversable1 (Lift f) where
  traverse1 f (Pure x)  = Pure <$> f x
  traverse1 f (Other y) = Other <$> traverse1 f y

instance Traversable1 f => Traversable1 (Reverse f) where
  traverse1 f = fmap Reverse . forwards . traverse1 (Backwards . f) . getReverse

instance Traversable1 Complex where
  traverse1 f (a :+ b) = (:+) <$> f a <.> f b
  {-# INLINE traverse1 #-}

#ifdef MIN_VERSION_tagged
instance Traversable1 (Tagged a) where
  traverse1 f (Tagged a) = Tagged <$> f a
#endif

#ifdef MIN_VERSION_containers
instance Traversable1 Tree where
  traverse1 f (Node a []) = (`Node`[]) <$> f a
  traverse1 f (Node a (x:xs)) = (\b (y:|ys) -> Node b (y:ys)) <$> f a <.> traverse1 (traverse1 f) (x :| xs)
#endif

instance Traversable1 NonEmpty where
  traverse1 f (a :| as) = foldr (\b g x -> (\a' (b':| bs') -> a' :| b': bs') <$> f x <.> g b) (fmap (:|[]) . f) as a

instance Traversable1 ((,) a) where
  traverse1 f (a, b) = (,) a <$> f b

instance Traversable1 g => Traversable1 (Joker g a) where
  traverse1 g = fmap Joker . traverse1 g . runJoker
  {-# INLINE traverse1 #-}

instance Traversable1 Monoid.Sum where
  traverse1 g (Monoid.Sum a) = Monoid.Sum <$> g a

instance Traversable1 Monoid.Product where
  traverse1 g (Monoid.Product a) = Monoid.Product <$> g a

instance Traversable1 Monoid.Dual where
  traverse1 g (Monoid.Dual a) = Monoid.Dual <$> g a

instance Traversable1 f => Traversable1 (Monoid.Alt f) where
  traverse1 g (Monoid.Alt m) = Monoid.Alt <$> traverse1 g m

instance Traversable1 Semigroup.First where
  traverse1 g (Semigroup.First a) = Semigroup.First <$> g a

instance Traversable1 Semigroup.Last where
  traverse1 g (Semigroup.Last a) = Semigroup.Last <$> g a

instance Traversable1 Semigroup.Min where
  traverse1 g (Semigroup.Min a) = Semigroup.Min <$> g a

instance Traversable1 Semigroup.Max where
  traverse1 g (Semigroup.Max a) = Semigroup.Max <$> g a
