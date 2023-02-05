{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Re-exports a subset of the "Data.Foldable1" module along with some additional
-- combinators that require 'Foldable1' constraints.
--
----------------------------------------------------------------------------
module Data.Semigroup.Foldable
  ( -- @Data.Foldable1@ re-exports
    Foldable1(fold1, foldMap1, toNonEmpty)
  , intercalate1
  , foldrM1
  , foldlM1

    -- Additional @Foldable1@ functionality
  , intercalateMap1
  , traverse1_
  , for1_
  , sequenceA1_
  , foldMapDefault1
  , asum1

    -- Generic defaults
  , gfold1
  , gfoldMap1
  , gtoNonEmpty
  ) where

import Data.Foldable
import Data.Foldable1
import Data.Functor.Alt (Alt(..))
import Data.Functor.Apply
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable.Instances ()
import Data.Semigroup hiding (Product, Sum)
import GHC.Generics
import Prelude hiding (foldr)

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Data.Monoid (Monoid (..))

newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-- | Insert @m@ between each pair of @m@ derived from @a@.
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

newtype Alt_ f a = Alt_ { getAlt_ :: f a }

instance Alt f => Semigroup (Alt_ f a) where
  Alt_ a <> Alt_ b = Alt_ (a <!> b)

asum1 :: (Foldable1 t, Alt m) => t (m a) -> m a
asum1 = getAlt_ . foldMap1 Alt_
{-# INLINE asum1 #-}

-- | Generic 'fold1'. Caveats:
--
--   1. Will not compile if @t@ is an empty constructor.
--   2. Will not compile if @t@ has some fields that don't mention @a@, for exmaple @data Bar a = MkBar a Int@
--
-- @since 5.3.8
gfold1 :: (Foldable1 (Rep1 t), Generic1 t, Semigroup m) => t m -> m
gfold1 = fold1 . from1

-- | Generic 'foldMap1'. Caveats are the same as for 'gfold1'.
--
-- @since 5.3.8
gfoldMap1 :: (Foldable1 (Rep1 t), Generic1 t, Semigroup m) => (a -> m) -> t a -> m
gfoldMap1 f = foldMap1 f . from1

-- | Generic 'toNonEmpty'. Caveats are the same as for 'gfold1'.
--
-- @since 5.3.8
gtoNonEmpty :: (Foldable1 (Rep1 t), Generic1 t) => t a -> NonEmpty a
gtoNonEmpty = toNonEmpty . from1
