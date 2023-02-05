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
----------------------------------------------------------------------------
module Data.Semigroup.Traversable
  ( Traversable1(..)
  -- * Defining Traversable1 instances
  -- $traversable1instances
  , traverse1Maybe
  , gtraverse1
  , gsequence1
  -- * Default superclass instance helpers
  , foldMap1Default
  ) where

import Control.Applicative
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Traversable.Class
import Data.Functor.Bind.Class
import GHC.Generics

-- | Default implementation of 'foldMap1' given an implementation of 'Traversable1'.
foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)

-- | Generic 'traverse1'. Caveats:
--
--   1. Will not compile if @t@ is an empty constructor.
--   2. Will not compile if @t@ has some fields that don't mention @a@, for exmaple @data Bar a = MkBar a Int@
--
-- @since 5.3.8
gtraverse1 ::
  (Traversable1 (Rep1 t), Apply f, Generic1 t) =>
  (a -> f b) ->
  t a ->
  f (t b)
gtraverse1 f x = to1 <$> traverse1 f (from1 x)

-- | Generic 'sequence1'. Caveats are the same for 'gtraverse1'.
--
-- @since 5.3.8
gsequence1 ::
  (Traversable1 (Rep1 t), Apply f, Generic1 t) =>
  t (f b) ->
  f (t b)
gsequence1 = fmap to1 . sequence1 . from1

-- $traversable1instances
-- Defining 'Traversable1' instances for types with both 'Traversable1' and 'Traversable'
-- substructures can be done with 'traverse1Maybe', '(<*.>)', and '(<.*>)'.
--
-- > data Foo a = Foo (Maybe a) (Maybe a) a [a]
-- >   deriving (Functor, Traversable, Foldable)
-- > instance Traversable1 Foo where
-- >   traverse1 f (Foo ma ma' a as) = Foo <$> traverseMaybe ma <*> traverseMaybe ma' <*.> f a <.*> traverseMaybe as
-- > instance Foldable1 Foo where
-- >   foldMap1 = foldMap1Default
