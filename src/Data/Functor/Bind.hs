{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
{-# OPTIONS_GHC -fno-warn-amp #-}
#endif
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
  , (-<<)
  , (-<-)
  , (->-)
  , apDefault
  , returning
  ) where

import Data.Functor.Apply
import Data.Functor.Bind.Class

infixr 1 -<<, -<-, ->-

(-<<) :: Bind m => (a -> m b) -> m a -> m b
(-<<) = flip (>>-)

(->-) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f ->- g = \a -> f a >>- g

(-<-) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g -<- f = \a -> f a >>- g


