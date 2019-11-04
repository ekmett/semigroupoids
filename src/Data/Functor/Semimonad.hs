{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL <= 706 && defined(MIN_VERSION_comonad) && !(MIN_VERSION_comonad(3,0,3))
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
module Data.Functor.Semimonad (
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b
  -- * Semiapplicativeable functors
  , Semiapplicative(..)
  , (<..>)    -- :: Semiapplicative w => w a -> w (a -> b) -> w b
  , liftF3    -- :: Semiapplicative w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeSemiapplicative(..)
  -- * Semimonadable functors
  , Semimonad(..)
  , (-<<)
  , (-<-)
  , (->-)
  , apDefault
  , returning
  ) where

import Data.Functor.Semiapplicative
import Data.Functor.Semimonad.Class

infixr 1 -<<, -<-, ->-

(-<<) :: Semimonad m => (a -> m b) -> m a -> m b
(-<<) = flip (>>-)

(->-) :: Semimonad m => (a -> m b) -> (b -> m c) -> a -> m c
f ->- g = \a -> f a >>- g

(-<-) :: Semimonad m => (b -> m c) -> (a -> m b) -> a -> m c
g -<- f = \a -> f a >>- g


