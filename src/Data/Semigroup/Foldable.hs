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
  ( NonEmptyFoldable(..)
  , intercalateNE
  , intercalateMapNE
  , traverseNE_
  , for1_
  , sequenceA1_
  , foldMapDefaultNE
  , asumNE
  , foldrMNE
  , foldlMNE
  ) where

import Data.Foldable
import Data.Functor.Semialternative (Semialternative(..))
import Data.Functor.Semiapplicative
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable.Instances ()
import Data.Semigroup hiding (Product, Sum)
import Data.Semigroup.Foldable.Class
import Prelude hiding (foldr)

-- $setup
-- >>> import Data.List.NonEmpty

newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-- | Insert an 'm' between each pair of 't m'.  Equivalent to
-- 'intercalateMapNE' with 'id' as the second argument.
--
-- >>> intercalateNE ", " $ "hello" :| ["how", "are", "you"]
-- "hello, how, are, you"
--
-- >>> intercalateNE ", " $ "hello" :| []
-- "hello"
--
-- >>> intercalateNE mempty $ "I" :| ["Am", "Fine", "You?"]
-- "IAmFineYou?"
intercalateNE :: (NonEmptyFoldable t, Semigroup m) => m -> t m -> m
intercalateNE = flip intercalateMapNE id
{-# INLINE intercalateNE #-}

-- | Insert 'm' between each pair of 'm' derived from 'a'.
--
-- >>> intercalateMapNE " " show $ True :| [False, True]
-- "True False True"
--
-- >>> intercalateMapNE " " show $ True :| []
-- "True"
intercalateMapNE :: (NonEmptyFoldable t, Semigroup m) => m -> (a -> m) -> t a -> m
intercalateMapNE j f = flip joinee j . foldMapNE (JoinWith . const . f)
{-# INLINE intercalateMapNE #-}

newtype Act f a = Act { getAct :: f a }

instance Semiapplicative f => Semigroup (Act f a) where
  Act a <> Act b = Act (a .> b)

instance Functor f => Functor (Act f) where
  fmap f (Act a) = Act (f <$> a)
  b <$ Act a = Act (b <$ a)

traverseNE_ :: (NonEmptyFoldable t, Semiapplicative f) => (a -> f b) -> t a -> f ()
traverseNE_ f t = () <$ getAct (foldMapNE (Act . f) t)
{-# INLINE traverseNE_ #-}

for1_ :: (NonEmptyFoldable t, Semiapplicative f) => t a -> (a -> f b) -> f ()
for1_ = flip traverseNE_
{-# INLINE for1_ #-}

sequenceA1_ :: (NonEmptyFoldable t, Semiapplicative f) => t (f a) -> f ()
sequenceA1_ t = () <$ getAct (foldMapNE Act t)
{-# INLINE sequenceA1_ #-}

-- | Usable default for foldMap, but only if you define foldMapNE yourself
foldMapDefaultNE :: (NonEmptyFoldable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefaultNE f = unwrapMonoid . foldMap (WrapMonoid . f)
{-# INLINE foldMapDefaultNE #-}

-- toStream :: NonEmptyFoldable t => t a -> Stream a
-- concat1 :: NonEmptyFoldable t => t (Stream a) -> Stream a
-- concatMap1 :: NonEmptyFoldable t => (a -> Stream b) -> t a -> Stream b

newtype Alt_ f a = Alt_ { getAlt_ :: f a }

instance Semialternative f => Semigroup (Alt_ f a) where
  Alt_ a <> Alt_ b = Alt_ (a <!> b)

asumNE :: (NonEmptyFoldable t, Semialternative m) => t (m a) -> m a
asumNE = getAlt_ . foldMapNE Alt_
{-# INLINE asumNE #-}

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
--
-- > let g = (=<<) . f
-- > in foldrMNE f (x1 :| [x2, ..., xn]) == x1 `g` (x2 `g` ... (xn-1 `f` xn)...)
--
foldrMNE :: (NonEmptyFoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrMNE f = go . toNonEmpty
  where
    g = (=<<) . f
    
    go (e:|es) =
      case es of
        []   -> return e
        x:xs -> e `g` (go (x:|xs))

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
--
-- > let g = flip $ (=<<) . f
-- > in foldlMNE f (x1 :| [x2, ..., xn]) == (...((x1 `f` x2) `g` x2) `g`...) `g` xn
--
foldlMNE :: (NonEmptyFoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlMNE f t = foldlM f x xs
  where
    x:|xs = toNonEmpty t
