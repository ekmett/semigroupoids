{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_comonad
#if MIN_VERSION_comonad(3,0,3)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#else
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  polykinds
--
----------------------------------------------------------------------------

module Data.Semifunctor
  ( Semifunctor(..)
  , Bi(..)
  , (#)
  , semibimap
  , semifirst
  , semisecond
  , first
  , second
  , WrappedFunctor(..)
  , WrappedTraversable1(..)
  , module Control.Category
  , module Data.Semigroupoid
  , module Data.Semigroupoid.Ob
  , module Data.Semigroupoid.Product
  ) where

import Control.Arrow hiding (first, second, left, right)
import Control.Category
import Control.Monad (liftM)
import Data.Functor.Bind
import Data.Traversable
import Data.Semigroup.Traversable
import Data.Semigroupoid
import Data.Semigroupoid.Dual
import Data.Semigroupoid.Ob
import Data.Semigroupoid.Product
import Prelude hiding ((.),id, mapM)

#ifdef MIN_VERSION_comonad
import Control.Comonad
import Data.Functor.Extend
#ifdef MIN_VERSION_distributive
import Data.Distributive
#endif
#endif

-- | Semifunctors map objects to objects, and arrows to arrows preserving connectivity
-- as normal functors, but do not purport to preserve identity arrows. We apply them
-- to semigroupoids, because those don't even claim to offer identity arrows!
class (Semigroupoid c, Semigroupoid d) => Semifunctor f c d | f c -> d, f d -> c where
  semimap :: c a b -> d (f a) (f b)

data WrappedFunctor f a = WrapFunctor { unwrapFunctor :: f a }

instance Functor f => Semifunctor (WrappedFunctor f) (->) (->) where
  semimap f = WrapFunctor . fmap f . unwrapFunctor

instance (Traversable f, Bind m, Monad m) => Semifunctor (WrappedFunctor f) (Kleisli m) (Kleisli m) where
  semimap (Kleisli f) = Kleisli $ liftM WrapFunctor . mapM f . unwrapFunctor

#if defined(MIN_VERSION_distributive) && defined(MIN_VERSION_comonad)
instance (Distributive f, Extend w) => Semifunctor (WrappedFunctor f) (Cokleisli w) (Cokleisli w) where
  semimap (Cokleisli w) = Cokleisli $ WrapFunctor . cotraverse w . fmap unwrapFunctor
#endif

data WrappedTraversable1 f a = WrapTraversable1 { unwrapTraversable1 :: f a }

instance (Traversable1 f, Bind m) => Semifunctor (WrappedTraversable1 f) (Kleisli m) (Kleisli m) where
  semimap (Kleisli f) = Kleisli $ fmap WrapTraversable1 . traverse1 f . unwrapTraversable1

-- | Used to map a more traditional bifunctor into a semifunctor
data Bi p a where
  Bi :: p a b -> Bi p '(a,b)

instance Semifunctor f c d => Semifunctor f (Dual c) (Dual d) where
  semimap (Dual f) = Dual (semimap f)

(#) :: a -> b -> Bi (,) '(a,b)
a # b = Bi (a,b)

#ifdef MIN_VERSION_comonad
fstP :: Bi (,) '(a, b) -> a
fstP (Bi (a,_)) = a

sndP :: Bi (,) '(a, b) -> b
sndP (Bi (_,b)) = b
#endif

left :: a -> Bi Either '(a,b)
left = Bi . Left

right :: b -> Bi Either '(a,b)
right = Bi . Right

instance Semifunctor (Bi (,)) (Product (->) (->)) (->) where
  semimap (Pair l r) (Bi (a,b)) = l a # r b

instance Semifunctor (Bi Either) (Product (->) (->)) (->) where
  semimap (Pair l _) (Bi (Left a)) = Bi (Left (l a))
  semimap (Pair _ r) (Bi (Right b)) = Bi (Right (r b))

instance Bind m => Semifunctor (Bi (,)) (Product (Kleisli m) (Kleisli m)) (Kleisli m) where
  semimap (Pair l r) = Kleisli (\ (Bi (a, b)) -> (#) <$> runKleisli l a <.> runKleisli r b)

instance Bind m => Semifunctor (Bi Either) (Product (Kleisli m) (Kleisli m)) (Kleisli m) where
  semimap (Pair (Kleisli l0) (Kleisli r0)) = Kleisli (lr l0 r0) where
    lr :: Functor m => (a -> m c) -> (b -> m d) -> Bi Either '(a,b) -> m (Bi Either '(c,d))
    lr l _ (Bi (Left a))  = left <$> l a
    lr _ r (Bi (Right b)) = right <$> r b

#ifdef MIN_VERSION_comonad
instance Extend w => Semifunctor (Bi (,)) (Product (Cokleisli w) (Cokleisli w)) (Cokleisli w) where
  semimap (Pair l r) = Cokleisli $ \p -> runCokleisli l (fstP <$> p) # runCokleisli r (sndP <$> p)

-- instance Extend w => Semifunctor (Bi Either)) (Product (Cokleisli w) (Cokleisli w)) (Cokleisli w) where
#endif

semibimap :: Semifunctor p (Product l r) cod => l a b -> r c d -> cod (p '(a,c)) (p '(b,d))
semibimap f g = semimap (Pair f g)

semifirst :: (Semifunctor p (Product l r) cod, Ob r c) => l a b -> cod (p '(a,c)) (p '(b,c))
semifirst f = semimap (Pair f semiid)

semisecond :: (Semifunctor p (Product l r) cod, Ob l a) => r b c -> cod (p '(a,b)) (p '(a,c))
semisecond f = semimap (Pair semiid f)

first :: (Semifunctor p (Product l r) cod, Category r) => l a b -> cod (p '(a,c)) (p '(b,c))
first f = semimap (Pair f id)

second :: (Semifunctor p (Product l r) cod, Category l) => r b c -> cod (p '(a,b)) (p '(a,c))
second f = semimap (Pair id f)
