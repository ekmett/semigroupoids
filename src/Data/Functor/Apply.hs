{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
# ifdef MIN_VERSION_comonad
#  if __GLASGOW_HASKELL__ >= 707 && (MIN_VERSION_comonad(3,0,3))
{-# LANGUAGE Safe #-}
#  else
{-# LANGUAGE Trustworthy #-}
#  endif
# else
{-# LANGUAGE Trustworthy #-}
# endif
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

import Control.Comonad
import Data.Functor.Bind.Class

infixl 4 <..>

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: Apply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}

-- | Lift a binary function into a comonad with zipping
liftF2 :: Apply w => (a -> b -> c) -> w a -> w b -> w c
liftF2 f a b = f <$> a <.> b
{-# INLINE liftF2 #-}

-- | Lift a ternary function into a comonad with zipping
liftF3 :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

