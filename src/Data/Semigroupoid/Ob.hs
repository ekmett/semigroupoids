{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Ob
-- Copyright   :  (C) 2011-2012 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (flexible MPTCs)
--
----------------------------------------------------------------------------
module Data.Semigroupoid.Ob where

import Data.Semigroupoid
import Data.Semigroupoid.Product
import Data.Semigroupoid.Coproduct
import Control.Comonad
import Data.Functor.Bind
import Data.Functor.Extend
import Control.Arrow

class Semigroupoid k => Ob k a where
  semiid :: k a a

instance (Ob l a, Ob r b) => Ob (Product l r) (a,b) where
  semiid = Pair semiid semiid

instance (Ob l a, Semigroupoid r)  => Ob (Coproduct l r) (L a) where
  semiid = L semiid

instance (Semigroupoid l, Ob r a) => Ob (Coproduct l r) (R a) where
  semiid = R semiid

instance (Bind m, Monad m) => Ob (Kleisli m) a where
  semiid = Kleisli return

instance (Extend w, Comonad w) => Ob (Cokleisli w) a where
  semiid = Cokleisli extract

instance Ob (->) a where
  semiid = id
