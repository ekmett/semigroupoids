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
  ( Foldable1(..)
  , intercalate1
  , intercalateMap1
  , traverse1_
  , for1_
  , sequenceA1_
  , foldMapDefault1
  , asum1
  , foldrM1
  , foldlM1
  ) where

import Data.Foldable
import Data.Functor.Alt (Alt(..))
import Data.Functor.Apply
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

newtype Alt_ f a = Alt_ { getAlt_ :: f a }

instance Alt f => Semigroup (Alt_ f a) where
  Alt_ a <> Alt_ b = Alt_ (a <!> b)

asum1 :: (Foldable1 t, Alt m) => t (m a) -> m a
asum1 = getAlt_ . foldMap1 Alt_
{-# INLINE asum1 #-}

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
--
-- > let g = (=<<) . f
-- > in foldrM1 f (x1 :| [x2, ..., xn]) == x1 `g` (x2 `g` ... (xn-1 `f` xn)...)
--
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 f = go . toNonEmpty
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
-- > in foldlM1 f (x1 :| [x2, ..., xn]) == (...((x1 `f` x2) `g` x2) `g`...) `g` xn
--
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 f t = foldlM f x xs
  where
    x:|xs = toNonEmpty t
