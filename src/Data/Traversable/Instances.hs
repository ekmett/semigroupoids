{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-- | Placeholders for missing instances of Traversable, until base catches up and adds them
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Traversable.Instances where

#if !(MIN_VERSION_transformers(0,3,0))
import Control.Monad.Trans.Identity
#endif

#if !((MIN_VERSION_transformers(0,3,0)) && (MIN_VERSION_base(4,7,0)))
import Data.Foldable
import Data.Traversable
#endif

#if !(MIN_VERSION_base(4,7,0))
import Control.Applicative
import Data.Monoid
#endif

#if !(MIN_VERSION_transformers(0,3,0))
instance Foldable m => Foldable (IdentityT m) where
  foldMap f = foldMap f . runIdentityT

instance Traversable m => Traversable (IdentityT m) where
  traverse f = fmap IdentityT . traverse f . runIdentityT
#endif

--------------------------------------

#if !(MIN_VERSION_base(4,7,0))
instance Foldable ((,) b) where
  foldMap f (_, a) = f a

instance Traversable ((,) b) where
  traverse f (b, a) = (,) b <$> f a

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right a) = f a

instance Traversable (Either a) where
  traverse _ (Left b) = pure (Left b)
  traverse f (Right a) = Right <$> f a

instance Foldable (Const m) where
  foldMap _ _ = mempty

instance Traversable (Const m) where
  traverse _ (Const m) = pure $ Const m
#endif
