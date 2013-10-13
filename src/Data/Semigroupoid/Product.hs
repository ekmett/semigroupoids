{-# LANGUAGE GADTs #-}
module Data.Semigroupoid.Product 
  ( Product(..)
  , distributeDualProduct
  , factorDualProduct
  ) where

import Data.Semigroupoid
import Data.Semigroupoid.Dual
import Data.Groupoid

data Product j k a b where
  Pair :: j a b -> k a' b' -> Product j k (a,a') (b,b')

instance (Semigroupoid j, Semigroupoid k) => Semigroupoid (Product j k) where
  Pair w x `o` Pair y z = Pair (w `o` y) (x `o` z)

instance (Groupoid j, Groupoid k) => Groupoid (Product j k) where
  inv (Pair w x) = Pair (inv w) (inv x)

distributeDualProduct :: Dual (Product j k) a b -> Product (Dual j) (Dual k) a b
distributeDualProduct (Dual (Pair l r)) = Pair (Dual l) (Dual r)

factorDualProduct :: Product (Dual j) (Dual k) a b -> Dual (Product j k) a b
factorDualProduct (Pair (Dual l) (Dual r)) = Dual (Pair l r)

