{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 Koz Ross
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Koz Ross <koz.ross@retro-freedom.nz>
-- Stability   :  Experimental
-- Portability :  GHC only
--
-- Provides a way to attach an identity to any semigroupoid.
----------------------------------------------------------------------------
module Data.Semigroupoid.Categorical (
  Categorical(..),
  runCategorical
  ) where

import Control.Category (Category (id, (.)))
import Data.Semigroupoid (Semigroupoid (o))
import Prelude ()

-- | Attaches an identity.
--
-- @since 5.3.6
data Categorical s a b where
  Id :: Categorical s a a
  Embed :: s a b -> Categorical s a b

-- | @since 5.3.6
instance (Semigroupoid s) => Semigroupoid (Categorical s) where
  Id `o` y = y
  x `o` Id = x
  Embed x `o` Embed y = Embed (x `o` y)

-- | @since 5.3.6
instance (Semigroupoid s) => Category (Categorical s) where
  id = Id
  (.) = o

-- | @since 5.3.6
runCategorical :: (a ~ b => r) -> (s a b -> r) -> Categorical s a b -> r
runCategorical r _  Id = r
runCategorical _ f (Embed x) = f x
