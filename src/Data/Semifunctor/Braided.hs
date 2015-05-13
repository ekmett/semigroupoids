{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

#ifdef MIN_VERSION_comonad
#if MIN_VERSION_comonad(3,0,3)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#else
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, GADTs
--
----------------------------------------------------------------------------
module Data.Semifunctor.Braided
  ( Braided(..)
  , kleisliBraid
#ifdef MIN_VERSION_comonad
  , cokleisliBraid
#endif
  , Symmetric
  , swap
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Data.Functor.Bind
import Data.Semifunctor
import Data.Semifunctor.Associative
-- import Data.Semigroupoid.Dual

#ifdef MIN_VERSION_comonad
import Control.Comonad
import Data.Functor.Extend
#endif

class Associative k p => Braided k p where
  braid :: k (p '(a,b)) (p '(b,a))

-- instance Braided k p => Braided (Dual k) p where braid = Dual braid

instance Braided (->) (Bi Either) where
  braid (Bi (Left a)) = Bi (Right a)
  braid (Bi (Right a)) = Bi (Left a)

instance Braided (->) (Bi (,)) where
  braid (Bi (a,b)) = Bi (b,a)

kleisliBraid :: (Monad m, Semifunctor p (Product (Kleisli m) (Kleisli m)) (Kleisli m), Braided (->) p) => Kleisli m (p '(a,b)) (p '(b,a))
kleisliBraid = Kleisli (return . braid)

instance (Bind m, Monad m) => Braided (Kleisli m) (Bi Either) where
  braid = kleisliBraid

instance (Bind m, Monad m) => Braided (Kleisli m) (Bi (,)) where
  braid = kleisliBraid

#ifdef MIN_VERSION_comonad
cokleisliBraid :: (Extend w, Comonad w, Semifunctor p (Product (Cokleisli w) (Cokleisli w)) (Cokleisli w), Braided (->) p) =>
                  Cokleisli w (p '(a,b)) (p '(b,a))
cokleisliBraid = Cokleisli (braid . extract)

instance (Extend w, Comonad w) => Braided (Cokleisli w) (Bi (,)) where
  braid = cokleisliBraid

-- instance Comonad w => Braided (Cokleisli w) (Bi Either) where braid = cokleisliBraid
#endif

class Braided k p => Symmetric k p
instance Symmetric (->) (Bi Either)
instance Symmetric (->) (Bi (,))
instance (Bind m, Monad m) => Symmetric (Kleisli m) (Bi Either)
instance (Bind m, Monad m) => Symmetric (Kleisli m) (Bi (,))
#ifdef MIN_VERSION_comonad
instance (Extend w, Comonad w) => Symmetric (Cokleisli w) (Bi (,))
-- instance Comonad w => Symmetric (Cokleisli w) (Bi Either)
#endif

swap :: Symmetric k p => k (p '(a,b)) (p '(b,a))
swap = braid
