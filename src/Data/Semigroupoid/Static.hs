{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  polykinds
--
----------------------------------------------------------------------------

module Data.Semigroupoid.Static
  ( Static(..)
  ) where

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad (ap)
import Data.Functor.Apply
import Data.Functor.Plus
import Data.Functor.Extend
import Data.Orphans ()
import Data.Semigroup
import Data.Semigroupoid
import Prelude hiding ((.), id)

#ifdef MIN_VERSION_comonad
import Control.Comonad
#endif

newtype Static f a b = Static { runStatic :: f (a -> b) }

instance Functor f => Functor (Static f a) where
  fmap f = Static . fmap (f .) . runStatic

instance Apply f => Apply (Static f a) where
  Static f <.> Static g = Static (ap <$> f <.> g)

instance Alt f => Alt (Static f a) where
  Static f <!> Static g = Static (f <!> g)

instance Plus f => Plus (Static f a) where
  zero = Static zero

instance Applicative f => Applicative (Static f a) where
  pure = Static . pure . const
  Static f <*> Static g = Static (ap <$> f <*> g)

instance (Extend f, Semigroup a) => Extend (Static f a) where
  extended f = Static . extended (\wf m -> f (Static (fmap (. (<>) m) wf))) . runStatic

#ifdef MIN_VERSION_comonad
instance (Comonad f, Monoid a) => Comonad (Static f a) where
  extend f = Static . extend (\wf m -> f (Static (fmap (. mappend m) wf))) . runStatic
  extract (Static g) = extract g mempty
#endif

instance Apply f => Semigroupoid (Static f) where
  Static f `o` Static g = Static ((.) <$> f <.> g)

instance Applicative f => Category (Static f) where
  id = Static (pure id)
  Static f . Static g = Static ((.) <$> f <*> g)

instance Applicative f => Arrow (Static f) where
  arr = Static . pure
  first (Static g) = Static (first <$> g)
  second (Static g) = Static (second <$> g)
  Static g *** Static h = Static ((***) <$> g <*> h)
  Static g &&& Static h = Static ((&&&) <$> g <*> h)

instance Alternative f => ArrowZero (Static f) where
  zeroArrow = Static empty

instance Alternative f => ArrowPlus (Static f) where
  Static f <+> Static g = Static (f <|> g)

instance Applicative f => ArrowChoice (Static f) where
  left (Static g) = Static (left <$> g)
  right (Static g) = Static (right <$> g)
  Static g +++ Static h = Static ((+++) <$> g <*> h)
  Static g ||| Static h = Static ((|||) <$> g <*> h)

