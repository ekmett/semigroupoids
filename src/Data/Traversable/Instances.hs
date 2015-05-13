{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  polykinds
--
-- Placeholders for missing instances of 'Traversable', and 'Foldable' until
-- base catches up and adds them. Many of these are re-exports from the
-- `base-orphans` package.
----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Traversable.Instances where

import Data.Orphans ()

#if !(MIN_VERSION_transformers(0,3,0))
import Control.Monad.Trans.Identity
import Data.Foldable
import Data.Traversable

instance Foldable m => Foldable (IdentityT m) where
  foldMap f = foldMap f . runIdentityT

instance Traversable m => Traversable (IdentityT m) where
  traverse f = fmap IdentityT . traverse f . runIdentityT
#endif
