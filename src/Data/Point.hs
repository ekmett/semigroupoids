module Data.Point
  ( InitialPoint
  , Point
  , mpoint
  , spoint_
  , spoint
  , ipoint
  , dpoint
  , maybeSAppend
  , flatSpoint
  , End(..)
  , ipsum
  , foldsum
  )
  where

--  Generalisation of sconcat and mconcat to spoint and mpoint respectively.
-- The names are given to represent the fact that an ordered collection of
-- semigroup or monoidal values define a single value - the "point".

import Data.Semigroup (Semigroup,(<>),sconcat)
import Data.Monoid (Monoid,mempty,mconcat,Sum(..),getSum)
import Data.Functor.Identity
import Data.Functor ((<$>))
import Data.List.NonEmpty (nonEmpty, NonEmpty(..))
import Data.Tree
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Control.Applicative ((<*>))
import Data.Foldable (toList, Foldable)

data End = Front | Back
endToCombinator :: End -> (a -> a -> b) -> a -> a -> b
endToCombinator Front = id
endToCombinator Back  = flip

-- extracts a value from a possibly empty collection using '<>'
-- doesn't have to fold over a sequence, can be parallel, etc
class InitialPoint p where
  -- if p contains monoids we can get a value even if p is empty
  mpoint  :: (Semigroup m, Monoid m) => p m -> m
  -- if p contains only semigroups then maybe we can
  spoint_ :: (Semigroup s) => p s -> Maybe s
  -- in case p is empty of semigroups
  -- put your own value at either the start or end
  ipoint  :: (Semigroup s) => End -> s -> p s -> s
  -- obviously Maybe can't use this definition
  ipoint end s = ipoint end s . spoint_
  -- or provide a default value
  dpoint  :: (Semigroup s) => s -> p s -> s
  dpoint default_ = fromMaybe default_ . spoint_

-- extracts a value from a nonempty collection of values using '<>'
-- doesn't have to fold over a sequence, can be parallel, etc
class InitialPoint p => Point p where
  -- if p contains only semigroups then, knowing it's nonempty
  -- we can get a single value
  spoint :: (Semigroup s) => p s -> s

ipsum :: (Num n, InitialPoint f, Functor f) => f n -> n
ipsum = getSum . mpoint . fmap Sum

-- equivalent to Foldable's sum
foldsum :: (Num n, Foldable f) => f n -> n
foldsum = ipsum . toList

--
-- semigroup append something in front of a nonempty list's spoint if there's
-- a list, else just the thing alone
maybeSAppend ::
  ( Point superiorP
  , InitialPoint inferiorP
  , Functor superiorP
  , Semigroup s
  ) => s -> Maybe (superiorP (inferiorP s)) -> s
maybeSAppend = maybe <$> id <*> flatSpoint Front

flatSpoint ::
  ( InitialPoint superiorP
  , InitialPoint inferiorP
  , Functor superiorP
  , Semigroup s
  ) => End -> s -> superiorP (inferiorP s) -> s
flatSpoint end h = ipoint end h . join . spoint_ . fmap spoint_

instance InitialPoint Maybe where
  mpoint = fromMaybe mempty
  spoint_ = id
  ipoint end = maybe <*> endToCombinator end (<>)


instance InitialPoint Identity where
  mpoint  = runIdentity
  spoint_ = Just . spoint

instance Point Identity where
  spoint = runIdentity


instance InitialPoint [] where
  mpoint = mconcat
  spoint_ p = spoint <$> nonEmpty p


instance InitialPoint NonEmpty where
  mpoint = sconcat
  spoint_ = Just . spoint

instance Point NonEmpty where
  spoint = sconcat


instance InitialPoint Tree where
  mpoint = spoint
  spoint_ = Just . spoint

instance Point Tree where
  spoint (Node a f)      = flatSpoint Front a f

