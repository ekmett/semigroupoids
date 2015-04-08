{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif
-- | Placeholders for missing instances of Traversable, until base catches up and adds them
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Traversable.Instances where

#if !(MIN_VERSION_transformers(0,3,0))
import Control.Monad.Trans.Identity
import Data.Foldable
import Data.Traversable
#endif

-- Imports orphan Foldable/Traversable instances for (,), Either, and Const
import Data.Traversable.Compat ()

#if !(MIN_VERSION_transformers(0,3,0))
instance Foldable m => Foldable (IdentityT m) where
  foldMap f = foldMap f . runIdentityT

instance Traversable m => Traversable (IdentityT m) where
  traverse f = fmap IdentityT . traverse f . runIdentityT
#endif
