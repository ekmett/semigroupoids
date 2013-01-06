-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroupoid
-- Copyright   :  (C) 2007-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A semigroupoid satisfies all of the requirements to be a Category except
-- for the existence of identity arrows.
----------------------------------------------------------------------------
module Data.Semigroupoid
  ( Semigroupoid(..)
  , WrappedCategory(..)
  , Semi(..)
  ) where

import Control.Arrow
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Contravariant
import Control.Comonad
import Data.Semigroup
import Control.Category
import Prelude hiding (id, (.))

-- | 'Control.Category.Category' sans 'Control.Category.id'
class Semigroupoid c where
  o :: c j k -> c i j -> c i k

instance Semigroupoid (->) where
  o = (.)

instance Bind m => Semigroupoid (Kleisli m) where
  Kleisli g `o` Kleisli f = Kleisli $ \a -> f a >>- g

instance Extend w => Semigroupoid (Cokleisli w) where
  Cokleisli f `o` Cokleisli g = Cokleisli $ f . extended g

instance Semigroupoid Op where
  Op f `o` Op g = Op (g `o` f)

newtype WrappedCategory k a b = WrapCategory { unwrapCategory :: k a b }

instance Category k => Semigroupoid (WrappedCategory k) where
  WrapCategory f `o` WrapCategory g = WrapCategory (f . g)

instance Category k => Category (WrappedCategory k) where
  id = WrapCategory id
  WrapCategory f . WrapCategory g = WrapCategory (f . g)

newtype Semi m a b = Semi { getSemi :: m }

instance Semigroup m => Semigroupoid (Semi m) where
  Semi m `o` Semi n = Semi (m <> n)

instance Monoid m => Category (Semi m) where
  id = Semi mempty
  Semi m . Semi n = Semi (m `mappend` n)
