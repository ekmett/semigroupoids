module Data.Semigroupoid (Semigroupoid(..)) where

import Control.Arrow
import Data.Functor.Bind
import Control.Comonad

-- | 'Control.Category.Category' sans 'Control.Category.id'
class Semigroupoid c where
  o :: c j k -> c i j -> c i k

instance Semigroupoid (->) where
  o = (.) 

instance Bind m => Semigroupoid (Kleisli m) where
  Kleisli g `o` Kleisli f = Kleisli $ \a -> f a >>- g

instance Extend w => Semigroupoid (Cokleisli w) where
  Cokleisli f `o` Cokleisli g = Cokleisli $ f . extend g
