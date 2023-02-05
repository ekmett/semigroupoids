{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroupoid
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
module Data.Semigroupoid
  ( Semigroupoid(..)
  , WrappedCategory(..)
  , Semi(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Bind
import Data.Semigroup
import qualified Data.Type.Coercion as Co
import qualified Data.Type.Equality as Eq
import Prelude hiding (id, (.))

#ifdef MIN_VERSION_contravariant
import Data.Functor.Contravariant
#endif

#ifdef MIN_VERSION_comonad
import Data.Functor.Extend
import Control.Comonad
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged (Tagged (..))
#endif

-- | 'Control.Category.Category' sans 'Control.Category.id'
class Semigroupoid c where
  o :: c j k -> c i j -> c i k

instance Semigroupoid (->) where
  o = (.)

-- | <http://en.wikipedia.org/wiki/Band_(mathematics)#Rectangular_bands>
instance Semigroupoid (,) where
  o (_,k) (i,_) = (i,k)

instance Bind m => Semigroupoid (Kleisli m) where
  Kleisli g `o` Kleisli f = Kleisli $ \a -> f a >>- g

#ifdef MIN_VERSION_comonad
instance Extend w => Semigroupoid (Cokleisli w) where
  Cokleisli f `o` Cokleisli g = Cokleisli $ f . extended g
#endif

#ifdef MIN_VERSION_contravariant
instance Semigroupoid Op where
  Op f `o` Op g = Op (g `o` f)
#endif

newtype WrappedCategory k a b = WrapCategory { unwrapCategory :: k a b }

instance Category k => Semigroupoid (WrappedCategory k) where
  WrapCategory f `o` WrapCategory g = WrapCategory (f . g)

instance Category k => Category (WrappedCategory k) where
  id = WrapCategory id
  WrapCategory f . WrapCategory g = WrapCategory (f . g)

newtype Semi m a b = Semi { getSemi :: m }

instance Semigroup m => Semigroupoid (Semi m) where
  Semi m `o` Semi n = Semi (m <> n)

instance Monoid m => Category (Semi m) where
  id = Semi mempty
  Semi m . Semi n = Semi (m `mappend` n)

instance Semigroupoid Const where
  _ `o` Const a = Const a

#ifdef MIN_VERSION_tagged
instance Semigroupoid Tagged where
  Tagged b `o` _ = Tagged b
#endif

instance Semigroupoid Co.Coercion where
  o = flip Co.trans

instance Semigroupoid (Eq.:~:) where
  o = flip Eq.trans

#if MIN_VERSION_base(4,10,0)
instance Semigroupoid (Eq.:~~:) where
  o Eq.HRefl Eq.HRefl = Eq.HRefl
#endif
