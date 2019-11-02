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
module Data.Semigroup.Foldable
  ( Semifoldable(..)
  , intercalate1
  , intercalateMap1
  , traverse1_
  , for1_
  , sequenceA1_
  , semifoldMapDefault
  , asum1
  , semifoldrM
  , semifoldlM
  , semifoldrMapM
  , semifoldlMapM
  , semiminimumBy
  , semimaximumBy
  ) where

import Data.Foldable
import Data.Functor.Alt (Alt(..))
import Data.Functor.Apply
import Data.Traversable.Instances ()
import Data.Semigroup hiding (Product, Sum)
import Prelude hiding (foldr)

import Data.Functor.Bind (Bind (..))
import Data.Semifoldable hiding (semifoldrMapM, semifoldlMapM)

-- $setup
-- >>> import Data.List.NonEmpty

newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-- | Insert 'm' between each pair of 'm' derived from 'a'.
--
-- >>> intercalateMap1 " " show $ True :| [False, True]
-- "True False True"
--
-- >>> intercalateMap1 " " show $ True :| []
-- "True"
intercalateMap1 :: (Semifoldable t, Semigroup m) => m -> (a -> m) -> t a -> m
intercalateMap1 j f = flip joinee j . semifoldMap (JoinWith . const . f)
{-# INLINE intercalateMap1 #-}
-- TODO, drop? should it be in semifoldable?

newtype Act f a = Act { getAct :: f a }

instance Apply f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  b <$ Act a = Act (b <$ a)

traverse1_ :: (Semifoldable t, Apply f) => (a -> f b) -> t a -> f ()
traverse1_ f t = () <$ getAct (semifoldMap (Act . f) t)
{-# INLINE traverse1_ #-}

for1_ :: (Semifoldable t, Apply f) => t a -> (a -> f b) -> f ()
for1_ = flip traverse1_
{-# INLINE for1_ #-}

sequenceA1_ :: (Semifoldable t, Apply f) => t (f a) -> f ()
sequenceA1_ t = () <$ getAct (semifoldMap Act t)
{-# INLINE sequenceA1_ #-}

-- | Usable default for foldMap, but only if you define semifoldMap yourself
semifoldMapDefault :: (Semifoldable t, Monoid m) => (a -> m) -> t a -> m
semifoldMapDefault f = unwrapMonoid . foldMap (WrapMonoid . f)
{-# INLINE semifoldMapDefault #-}

-- toStream :: Semifoldable t => t a -> Stream a
-- concat1 :: Semifoldable t => t (Stream a) -> Stream a
-- concatMap1 :: Semifoldable t => (a -> Stream b) -> t a -> Stream b

newtype Alt_ f a = Alt_ { getAlt_ :: f a }

instance Alt f => Semigroup (Alt_ f a) where
  Alt_ a <> Alt_ b = Alt_ (a <!> b)

asum1 :: (Semifoldable t, Alt m) => t (m a) -> m a
asum1 = getAlt_ . semifoldMap Alt_
{-# INLINE asum1 #-}

-- | Map variant of 'semifoldrM'.
--
-- This variant requires on;y 'Bind', not 'Monad'.
--
-- >>> semifoldrMapM print (\a _ -> print a) ('x' :| "yz")
-- 'z'
-- 'y'
-- 'x'
semifoldrMapM :: (Semifoldable t, Bind m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
semifoldrMapM g f = semifoldrMap g (\a m -> m >>- \b -> f a b)

-- | Map variant of 'semifoldlM'.
--
-- This variant requires on;y 'Bind', not 'Monad'.
--
-- >>> semifoldlMapM print (const print) ('x' :| "yz")
-- 'x'
-- 'y'
-- 'z'
semifoldlMapM :: (Semifoldable t, Bind m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
semifoldlMapM g f = semifoldlMap g (\m a -> m >>- \b -> f b a)
