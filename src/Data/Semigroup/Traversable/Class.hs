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
module Data.Semigroup.Traversable.Class
  ( Bitraversable1(..)
  , Traversable1(..)
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
import Data.Functor.Apply
import Data.Functor.Compose

#ifdef MIN_VERSION_comonad
import Data.Functor.Coproduct as Functor
#endif

import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Functor.Sum as Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Bifoldable
import Data.Tagged
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import Data.Traversable.Instances ()

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

class (Bifoldable1 t, Bitraversable t) => Bitraversable1 t where
  bitraverse1 :: Apply f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
  bitraverse1 f g  = bisequence1 . bimap f g
  {-# INLINE bitraverse1 #-}

  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 id id
  {-# INLINE bisequence1 #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bitraverse1 | bisequence1 #-}
#endif

#if MIN_VERSION_semigroups(0,16,2)
instance Bitraversable1 Arg where
  bitraverse1 f g (Arg a b) = Arg <$> f a <.> g b
#endif

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

instance Bitraversable1 Tagged where
  bitraverse1 _ g (Tagged b) = Tagged <$> g b
  {-# INLINE bitraverse1 #-}

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

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL traverse1 | sequence1 #-}
#endif

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

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Product f g) where
  traverse1 f (Functor.Pair a b) = Functor.Pair <$> traverse1 f a <.> traverse1 f b

instance Traversable1 f => Traversable1 (Reverse f) where
  traverse1 f = fmap Reverse . forwards . traverse1 (Backwards . f) . getReverse

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Sum f g) where
  traverse1 f (Functor.InL x) = Functor.InL <$> traverse1 f x
  traverse1 f (Functor.InR y) = Functor.InR <$> traverse1 f y

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

instance Traversable1 g => Traversable1 (Joker g a) where
  traverse1 g = fmap Joker . traverse1 g . runJoker
  {-# INLINE traverse1 #-}
