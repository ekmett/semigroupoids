{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (flexible MPTCs)
--
----------------------------------------------------------------------------
module Data.Semigroupoid.Ob where

import Data.Semigroupoid
import Data.Functor.Semimonad
import Control.Arrow


#ifdef MIN_VERSION_comonad
import Data.Functor.Extend
import Control.Comonad
#endif

class Semigroupoid k => Ob k a where
  semiid :: k a a

instance (Semimonad m, Monad m) => Ob (Kleisli m) a where
  semiid = Kleisli return

#ifdef MIN_VERSION_comonad
instance (Extend w, Comonad w) => Ob (Cokleisli w) a where
  semiid = Cokleisli extract
#endif

instance Ob (->) a where
  semiid = id
