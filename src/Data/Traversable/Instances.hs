{-# language CPP #-}
{-# language DeriveTraversable #-}
{-# language StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-orphans #-}
#endif
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
--
-- Also adds missing instances for 'Data.Monoid.Alt' until
-- <https://ghc.haskell.org/trac/ghc/ticket/15099> gets resolved
----------------------------------------------------------------------------
module Data.Traversable.Instances where

import Control.Monad.Trans.Instances ()
import Data.Orphans ()

#if MIN_VERSION_base(4,8,0)
import Data.Monoid
deriving instance Foldable f => Foldable (Alt f)
deriving instance Traversable f => Traversable (Alt f)
#endif
