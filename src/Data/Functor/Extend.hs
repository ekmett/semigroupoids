{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

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
  , gduplicated
  , gextended
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Functor.Sum as Functor (Sum(..))
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Orphans ()
import qualified Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import GHC.Generics as Generics

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

  {-# MINIMAL duplicated | extended #-}

-- | Generic 'duplicated'. Caveats:
--
--   1. Will not compile if @w@ is a product type.
--   2. Will not compile if @w@ contains fields where the type variable appears underneath the composition of type constructors (e.g., @f (g a)@).
--
-- @since 5.3.8
gduplicated :: (Extend (Rep1 w), Generic1 w) => w a -> w (w a)
gduplicated = to1 . fmap to1 . duplicated . from1

-- | Generic 'extended'. Caveats are the same as for 'gduplicated'.
--
-- @since 5.3.8
gextended :: (Extend (Rep1 w), Generic1 w) => (w a -> b) -> w a -> w b
gextended f = to1 . extended (f . to1) . from1

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

instance Extend Proxy where
  duplicated _ = Proxy
  extended _ _ = Proxy

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
  extended f w@(~(_ :| aas)) =
    f w :| case aas of
      []     -> []
      (a:as) -> toList (extended f (a :| as))

instance (Extend f, Extend g) => Extend (Functor.Sum f g) where
  extended f (InL l) = InL (extended (f . InL) l)
  extended f (InR r) = InR (extended (f . InR) r)

instance (Extend f, Extend g) => Extend (f :+: g) where
  extended f (L1 l) = L1 (extended (f . L1) l)
  extended f (R1 r) = R1 (extended (f . R1) r)

-- | @since 5.3.8
instance Extend (Generics.K1 i c) where
  duplicated (K1 c) = K1 c

instance Extend Generics.U1 where
  extended _ U1 = U1

instance Extend Generics.V1 where
  extended _ e = case e of {}

instance Extend f => Extend (Generics.M1 i t f) where
  extended f = M1 . extended (f . M1) . unM1

instance Extend Par1 where
  extended f w@Par1{} = Par1 (f w)

instance Extend f => Extend (Rec1 f) where
  extended f = Rec1 . extended (f . Rec1) . unRec1

instance Extend Monoid.Sum where
  extended f w@Monoid.Sum{} = Monoid.Sum (f w)

instance Extend Monoid.Product where
  extended f w@Monoid.Product{} = Monoid.Product (f w)

instance Extend Monoid.Dual where
  extended f w@Monoid.Dual{} = Monoid.Dual (f w)

instance Extend f => Extend (Monoid.Alt f) where
  extended f = Monoid.Alt . extended (f . Monoid.Alt) . Monoid.getAlt

-- in GHC 8.6 we'll have to deal with Apply f => Apply (Ap f) the same way
instance Extend Semigroup.First where
  extended f w@Semigroup.First{} = Semigroup.First (f w)

instance Extend Semigroup.Last where
  extended f w@Semigroup.Last{} = Semigroup.Last (f w)

instance Extend Semigroup.Min where
  extended f w@Semigroup.Min{} = Semigroup.Min (f w)

instance Extend Semigroup.Max where
  extended f w@Semigroup.Max{} = Semigroup.Max (f w)

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
