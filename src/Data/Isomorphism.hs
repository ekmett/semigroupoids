{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Isomorphism
  ( Iso(..)
  ) where

import Data.Semigroupoid
import Data.Groupoid
import Control.Category
import Prelude ()

data Iso k a b = Iso { embed :: k a b, project :: k b a }

instance Semigroupoid k => Semigroupoid (Iso k) where
  Iso f g `o` Iso h i = Iso (f `o` h) (i `o` g)

instance Semigroupoid k => Groupoid (Iso k) where
  inv (Iso f g) = Iso g f

instance Category k => Category (Iso k) where
  Iso f g . Iso h i = Iso (f . h) (i . g)
  id = Iso id id
