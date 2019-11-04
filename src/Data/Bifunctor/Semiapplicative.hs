{-# LANGUAGE CPP #-}
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
module Data.Bifunctor.Semiapplicative (
  -- * Biappliable bifunctors
    Bifunctor(..)
  , Bisemiapplicative(..)
  , (<<$>>)
  , (<<..>>)
  , bilift2
  , bilift3
  ) where

import Data.Functor.Semimonad.Class
import Data.Biapplicative

infixl 4 <<..>>

(<<..>>) :: Bisemiapplicative p => p a c -> p (a -> b) (c -> d) -> p b d
(<<..>>) = bilift2 (flip id) (flip id)
{-# INLINE (<<..>>) #-}

-- | Lift binary functions
bilift2 :: Bisemiapplicative w => (a -> b -> c) -> (d -> e -> f) -> w a d -> w b e -> w c f
bilift2 f g a b = bimap f g <<$>> a <<.>> b
{-# INLINE bilift2 #-}

-- | Lift ternary functions
bilift3 :: Bisemiapplicative w => (a -> b -> c -> d) -> (e -> f -> g -> h) -> w a e -> w b f -> w c g -> w d h
bilift3 f g a b c = bimap f g <<$>> a <<.>> b <<.>> c
{-# INLINE bilift3 #-}
