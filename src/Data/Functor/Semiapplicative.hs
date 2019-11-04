{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL <= 706 && defined(MIN_VERSION_comonad) && !(MIN_VERSION_comonad(3,0,3))
{-# LANGUAGE Trustworthy #-}
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
module Data.Functor.Semiapplicative (
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b

  -- * Semiapplicative - a strong lax semimonoidal endofunctor

  , Semiapplicative(..)
  , (<..>)    -- :: Semiapplicative w => w a -> w (a -> b) -> w b
  , liftF3    -- :: Semiapplicative w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeSemiapplicative(..)
  ) where

import Control.Comonad
import Data.Functor.Semimonad.Class

infixl 4 <..>

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: Semiapplicative w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}


-- | Lift a ternary function into a comonad with zipping
liftF3 :: Semiapplicative w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

