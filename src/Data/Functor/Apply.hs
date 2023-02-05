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
module Data.Functor.Apply (
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b

  -- * Apply - a strong lax semimonoidal endofunctor

  , Apply(..)
  , (<..>)    -- :: Apply w => w a -> w (a -> b) -> w b
  , liftF3    -- :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  , gliftF2
  , gliftF3

  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  , (<.*>)
  , (<*.>)
  ) where

import Data.Functor
import Data.Functor.Bind.Class
import GHC.Generics

infixl 4 <..>

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: Apply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}


-- | Lift a ternary function into a comonad with zipping
liftF3 :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

-- | Generic 'liftF2'. Caveats:
--
--   1. Will not compile if @w@ is a sum type.
--   2. Types in @w@ that do not mention the type variable must be instances of 'Semigroup'.
--
-- @since 5.3.8
gliftF2 :: (Generic1 w, Apply (Rep1 w)) => (a -> b -> c) -> w a -> w b -> w c
gliftF2 f wa wb = to1 $ liftF2 f (from1 wa) (from1 wb)

-- | Generic 'liftF3'. Caveats are the same as for 'gliftF2'.
--
-- @since 5.3.8
gliftF3 :: (Generic1 w, Apply (Rep1 w)) => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
gliftF3 f wa wb wc = to1 $ liftF3 f (from1 wa) (from1 wb) (from1 wc)
