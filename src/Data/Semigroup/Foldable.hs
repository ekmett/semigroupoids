{-# LANGUAGE CPP #-}
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
  , intercalate1
  , intercalateMap1
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable.Instances ()
import Data.Semigroup hiding (Product)
import Prelude hiding (foldr)

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

#ifdef MIN_VERSION_comonad
import Data.Functor.Coproduct
#endif

class Foldable t => Foldable1 t where
  fold1 :: Semigroup m => t m -> m
  foldMap1 :: Semigroup m => (a -> m) -> t a -> m

  foldMap1 f = maybe (error "foldMap1") id . getOption . foldMap (Option . Just . f)
  fold1 = foldMap1 id

#ifdef MIN_VERSION_containers
instance Foldable1 Tree where
  foldMap1 f (Node a []) = f a
  foldMap1 f (Node a (x:xs)) = f a <> foldMap1 (foldMap1 f) (x :| xs)
#endif

instance Foldable1 Identity where
  foldMap1 f = f . runIdentity

instance Foldable1 m => Foldable1 (IdentityT m) where
  foldMap1 f = foldMap1 f . runIdentityT

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
  foldMap1 f = foldMap1 (foldMap1 f) . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
  foldMap1 f (Pair a b) = foldMap1 f a <> foldMap1 f b

#ifdef MIN_VERSION_comonad
instance (Foldable1 f, Foldable1 g) => Foldable1 (Coproduct f g) where
  foldMap1 f = coproduct (foldMap1 f) (foldMap1 f)
#endif

instance Foldable1 NonEmpty where
  foldMap1 f (a :| []) = f a
  foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)

instance Foldable1 ((,) a) where
  foldMap1 f (_, x) = f x

newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-- | Insert an 'm' between each pair of 't m'.  Equivalent to
-- 'intercalateMap1' with 'id' as the second argument.
--
-- >>> intercalate1 ", " $ "hello" :| ["how", "are", "you"]
-- "hello, how, are, you"
--
-- >>> intercalate1 ", " $ "hello" :| []
-- "hello"
--
-- >>> intercalate1 mempty $ "I" :| ["Am", "Fine", "You?"]
-- "IAmFineYou?"
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 = flip intercalateMap1 id
{-# INLINE intercalate1 #-}

-- | Insert 'm' between each pair of 'm' derived from 'a'.
--
-- >>> intercalateMap1 " " show $ True :| [False, True]
-- "True False True"
--
-- >>> intercalateMap1 " " show $ True :| []
-- "True"
intercalateMap1 :: (Foldable1 t, Semigroup m) => m -> (a -> m) -> t a -> m
intercalateMap1 j f = flip joinee j . foldMap1 (JoinWith . const . f)
{-# INLINE intercalateMap1 #-}

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
