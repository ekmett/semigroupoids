{-# LANGUAGE FlexibleContexts #-}
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
module Data.Functor.Bind (
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b
  -- * Applyable functors
  , Apply(..)
  , (<..>)    -- :: Apply w => w a -> w (a -> b) -> w b
  , liftF3    -- :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  -- * Bindable functors
  , Bind(..)
  , gbind
  , (-<<)
  , (-<-)
  , (->-)
  , apDefault
  , returning
  ) where

import Data.Functor.Apply
import Data.Functor.Bind.Class
import GHC.Generics

-- | Generic '(>>-)'. Caveats:
--
--   1. Will not compile if @m@ is a sum type.
--   2. Will not compile if @m@ contains fields that do not mention its type variable.
--   3. Will not compile if @m@ contains fields where the type variable appears underneath the composition of type constructors (e.g., @f (g a)@).
--   4. May do redundant work, due to the nature of the 'Bind' instance for (':*:')
--
-- @since 5.3.8
gbind :: (Generic1 m, Bind (Rep1 m)) => m a -> (a -> m b) -> m b
gbind m f = to1 $ from1 m >>- (\a -> from1 $ f a)

infixr 1 -<<, -<-, ->-

(-<<) :: Bind m => (a -> m b) -> m a -> m b
(-<<) = flip (>>-)

(->-) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f ->- g = \a -> f a >>- g

(-<-) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g -<- f = \a -> f a >>- g
