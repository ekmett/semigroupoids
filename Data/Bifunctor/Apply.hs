-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Apply
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Apply ( 
  -- * Functors
  -- * Applyable bifunctors
    Biapply(..)
  , (<<$>>)
  , (<<..>>)
  , bilift2
  , bilift3
  , module Data.Bifunctor
  ) where

-- import _everything_
import Data.Bifunctor

infixl 4 <<$>>, <<.>>, <<., .>>, <<..>>

(<<$>>) :: (a -> b) -> a -> b
(<<$>>) = id

-- | A strong lax semi-monoidal endofunctor. 
-- This is equivalent to an 'Applicative' without 'pure'.
-- 
-- Laws: 
--
-- > associative composition: (.) <$> u <.> v <.> w = u <.> (v <.> w)
class Bifunctor p => Biapply p where
  (<<.>>) :: p (a -> b) (c -> d) -> p a c -> p b d

  -- | a .> b = const id <$> a <.> b
  (.>>) :: p a b -> p c d -> p c d
  a .>> b = bimap (const id) (const id) <<$>> a <<.>> b

  -- | a <. b = const <$> a <.> b
  (<<.) :: p a b -> p c d -> p a b
  a <<. b = bimap const const <<$>> a <<.>> b

(<<..>>) :: Biapply p => p a c -> p (a -> b) (c -> d) -> p b d
(<<..>>) = bilift2 (flip id) (flip id) 

-- | Lift a binary function into a comonad with zipping
bilift2 :: Biapply w => (a -> b -> c) -> (d -> e -> f) -> w a d -> w b e -> w c f
bilift2 f g a b = bimap f g <<$>> a <<.>> b
{-# INLINE bilift2 #-}

-- | Lift a ternary function into a comonad with zipping
bilift3 :: Biapply w => (a -> b -> c -> d) -> (e -> f -> g -> h) -> w a e -> w b f -> w c g -> w d h
bilift3 f g a b c = bimap f g <<$>> a <<.>> b <<.>> c
{-# INLINE bilift3 #-}

instance Biapply (,) where
  (f, g) <<.>> (a, b) = (f a, g b)
