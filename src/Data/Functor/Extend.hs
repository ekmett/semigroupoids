{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL <= 706 && defined(MIN_VERSION_comonad) && !(MIN_VERSION_comonad(3,0,3))
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Extend
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Extend
  ( -- * Extendable Functors
    -- $definition
    Extend(..)
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Functor.Sum (Sum(..))
import Data.Semigroup (Semigroup(..))
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..), toList)

#ifdef MIN_VERSION_containers
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tree
#endif


#ifdef MIN_VERSION_comonad
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if defined(MIN_VERSION_tagged) || MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif


class Functor w => Extend w where
  -- |
  -- > duplicated = extended id
  -- > fmap (fmap f) . duplicated = duplicated . fmap f
  duplicated :: w a -> w (w a)
  -- |
  -- > extended f  = fmap f . duplicated
  extended    :: (w a -> b) -> w a -> w b

  extended f = fmap f . duplicated
  duplicated = extended id

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL duplicated | extended #-}
#endif

-- * Extends for Prelude types:
--
-- Instances: While Data.Functor.Extend.Instances would be symmetric
-- to the definition of Control.Monad.Instances in base, the reason
-- the latter exists is because of Haskell 98 specifying the types
-- @'Either' a@, @((,)m)@ and @((->)e)@ and the class Monad without
-- having the foresight to require or allow instances between them.
--
-- Here Haskell 98 says nothing about Extend, so we can include the
-- instances directly avoiding the wart of orphan instances.

instance Extend [] where
  duplicated = init . tails

#ifdef MIN_VERSION_tagged
instance Extend (Tagged a) where
  duplicated = Tagged
#endif

#if defined(MIN_VERSION_tagged) || MIN_VERSION_base(4,7,0)
instance Extend Proxy where
  duplicated _ = Proxy
  extended _ _ = Proxy
#endif

instance Extend Maybe where
  duplicated Nothing = Nothing
  duplicated j = Just j

instance Extend (Either a) where
  duplicated (Left a) = Left a
  duplicated r = Right r

instance Extend ((,)e) where
  duplicated p = (fst p, p)

instance Semigroup m => Extend ((->)m) where
  duplicated f m = f . (<>) m

#ifdef MIN_VERSION_containers
instance Extend Seq where
  duplicated l = Seq.take (Seq.length l) (Seq.tails l)

instance Extend Tree where
  duplicated w@(Node _ as) = Node w (map duplicated as)
#endif

#ifdef MIN_VERSION_comonad
{-
instance (Extend f, Extend g) => Extend (Coproduct f g) where
  extended f = Coproduct . coproduct
    (Left . extended (f . Coproduct . Left))
    (Right . extended (f . Coproduct . Right))
-}

instance Extend w => Extend (EnvT e w) where
  duplicated (EnvT e wa) = EnvT e (extended (EnvT e) wa)

instance Extend w => Extend (StoreT s w) where
  duplicated (StoreT wf s) = StoreT (extended StoreT wf) s
  extended f (StoreT wf s) = StoreT (extended (\wf' s' -> f (StoreT wf' s')) wf) s

instance (Extend w, Semigroup m) => Extend (TracedT m w) where
  extended f = TracedT . extended (\wf m -> f (TracedT (fmap (. (<>) m) wf))) . runTracedT
#endif

-- I can't fix the world
-- instance (Monoid m, Extend n) => Extend (ReaderT m n)
--   duplicate f m = f . mappend m

-- * Extends for types from 'transformers'.
--
-- This isn't really a transformer, so i have no compunction about including the instance here.
--
-- TODO: Petition to move Data.Functor.Identity into base
instance Extend Identity where
  duplicated = Identity

-- Provided to avoid an orphan instance. Not proposed to standardize.
-- If Extend moved to base, consider moving instance into transformers?
instance Extend w => Extend (IdentityT w) where
  extended f (IdentityT m) = IdentityT (extended (f . IdentityT) m)

instance Extend NonEmpty where
  extended f w@ ~(_ :| aas) = f w :| case aas of
      []     -> []
      (a:as) -> toList (extended f (a :| as))

instance (Extend f, Extend g) => Extend (Sum f g) where
  extended f (InL l) = InL (extended (f . InL) l)
  extended f (InR r) = InR (extended (f . InR) r)

-- $definition
-- There are two ways to define an 'Extend' instance:
--
-- I. Provide definitions for 'extended'
-- satisfying this law:
--
-- > extended f . extended g = extended (f . extended g)
--
-- II. Alternately, you may choose to provide definitions for 'duplicated'
-- satisfying this law:
--
-- > duplicated . duplicated = fmap duplicated . duplicated
--
-- You may of course, choose to define both 'duplicated' /and/ 'extended'.
-- In that case you must also satisfy these laws:
--
-- > extended f = fmap f . duplicated
-- > duplicated = extended id
--
-- These are the default definitions of 'extended' and 'duplicated'.
