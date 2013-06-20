{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Apply
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Apply ( 
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b 

  -- * Apply - a strong lax semimonoidal endofunctor

  , Apply(..)
  , (<..>)    -- :: Apply w => w a -> w (a -> b) -> w b
  , liftF2    -- :: Apply w => (a -> b -> c) -> w a -> w b -> w c
  , liftF3    -- :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  ) where

import Data.Functor.Bind
