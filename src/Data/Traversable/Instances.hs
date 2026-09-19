{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015,2018 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  polykinds
--
-- Re-exports from the `base-orphans` and `transformers-compat` packages.
----------------------------------------------------------------------------
module Data.Traversable.Instances
  {-# DEPRECATED
        [ "This is an empty module that simply re-exports orphan instances "
        , "from the Data.Orphans module (from the base-orphans library) and "
        , "the Control.Monad.Trans.Instances module (from the "
        , "transformers-compat library). Import directly from these libraries "
        , "instead."
        ]
    #-}
  where

import Control.Monad.Trans.Instances ()
import Data.Orphans ()
