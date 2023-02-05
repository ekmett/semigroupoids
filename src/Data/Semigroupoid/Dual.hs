{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2007-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A semigroupoid satisfies all of the requirements to be a Category except
-- for the existence of identity arrows.
----------------------------------------------------------------------------
module Data.Semigroupoid.Dual (Dual(..)) where

import Data.Semigroupoid
import Control.Category
import Prelude ()

newtype Dual k a b = Dual { getDual :: k b a }

instance Semigroupoid k => Semigroupoid (Dual k) where
  Dual f `o` Dual g = Dual (g `o` f)

instance Category k => Category (Dual k) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)
