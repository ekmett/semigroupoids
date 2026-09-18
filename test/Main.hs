{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.Functor.Apply
import Data.Semigroup.Traversable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import GHC.Generics
import Test.Tasty.Bench

data V2 a = V2 a a
    deriving stock (Functor, Foldable, Generic1)
    deriving Apply via Generically1 V2

main :: IO ()
main = defaultMain runs where
    runs = [
        bench "traverse1" $ flip whnf xs $ sum . fmap sum . traverse1 id,
        bench "sequence1" $ flip whnf xs $ sum . fmap sum . sequence1]
    xs :: NonEmpty (V2 Double)
    xs = (\ a -> V2 (a + 5) (3 * a)) <$> N.fromList [1 .. 1000]
