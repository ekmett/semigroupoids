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
module Data.Semigroupoid (Semigroupoid(..)) where

import Control.Arrow
import Data.Functor.Bind
import Control.Comonad
import Data.Functor.Contravariant

-- | 'Control.Category.Category' sans 'Control.Category.id'
class Semigroupoid c where
  o :: c j k -> c i j -> c i k

instance Semigroupoid (->) where
  o = (.) 

instance Bind m => Semigroupoid (Kleisli m) where
  Kleisli g `o` Kleisli f = Kleisli $ \a -> f a >>- g

instance Extend w => Semigroupoid (Cokleisli w) where
  Cokleisli f `o` Cokleisli g = Cokleisli $ f . extend g

instance Semigroupoid Op where
  Op f `o` Op g = Op (g `o` f)
