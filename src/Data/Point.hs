module Data.Point
  ( InitialPoint
  , Point
  , mpoint
  , spoint_
  , spoint
  )
  where

--  Generalisation of sconcat and mconcat to spoint and mpoint respectively.
-- The names are given to represent the fact that an ordered collection of
-- semigroup or monoidal values define a single value - the "point".

import Data.Semigroup (Semigroup,(<>),sconcat)
import Data.Monoid (Monoid,mempty,mconcat)
import Data.Functor.Identity
import Data.List.NonEmpty (nonEmpty, NonEmpty(..))
import Data.Tree
import Data.Maybe (fromMaybe)

-- extracts a value from a possibly empty collection using '<>'
-- doesn't have to fold over a sequence, can be parallel, etc
class InitialPoint p where
  -- if p contains monoids we can get a value even if p is empty
  mpoint :: (Semigroup m, Monoid m) => p m -> m
  -- if p contains only semigroups then maybe we can
  spoint_ :: (Semigroup s) => p s -> Maybe s

-- extracts a value from a nonempty collection of values using '<>'
-- doesn't have to fold over a sequence, can be parallel, etc
class InitialPoint p => Point p where
  -- if p contains only semigroups then, knowing it's nonempty
  -- we can get a single value
  spoint :: (Semigroup s) => p s -> s


instance InitialPoint Identity where
  mpoint = runIdentity
  spoint_ = Just . spoint

instance Point Identity where
  spoint = runIdentity



instance InitialPoint [] where
  mpoint = mconcat
  spoint_ p = sconcat <$> nonEmpty p


instance InitialPoint NonEmpty where
  mpoint = sconcat
  spoint_ = Just . spoint

instance Point NonEmpty where
  spoint = sconcat


instance InitialPoint Tree where
  mpoint = spoint
  spoint_ = Just . spoint

instance Point Tree where
  spoint (Node a [])     = a
  spoint (Node a (f:fs)) = a <> (sconcat . fmap spoint) (f:|fs)


instance InitialPoint Maybe where
  mpoint = fromMaybe mempty
  spoint_ = id

