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
module Data.Semigroup.Foldable.Class
  {-# DEPRECATED
        [ "This module re-exports a limited subset of the class methods in the "
        , "Foldable1 and Bifoldable1 classes, which are now located in the "
        , "Data.Foldable1 and Data.Bifoldable1 modules in base-4.18. "
        , "(On older versions of base, these can be found in the "
        , "foldable1-classes-compat library.) "
        , "Import from these modules instead."
        ]
    #-}
  ( Foldable1(fold1, foldMap1, toNonEmpty)
  , Bifoldable1(bifold1, bifoldMap1)
  ) where

import Data.Bifoldable1
import Data.Foldable1
