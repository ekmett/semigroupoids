{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE EmptyCase     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Contravariant.Divise (
    Divise(..)
  , gdivise
  , divised
  , gdivised
  , WrappedDivisible(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Arrow
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import Data.Functor.Apply
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Monoid (Alt(..))
import Data.Proxy
import GHC.Generics

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
#endif

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup(..))
#endif

#ifdef MIN_VERSION_StateVar
import Data.StateVar
#endif

-- | The contravariant analogue of 'Apply'; it is
-- 'Divisible' without 'conquer'.
--
-- If one thinks of @f a@ as a consumer of @a@s, then 'divise' allows one
-- to handle the consumption of a value by splitting it between two
-- consumers that consume separate parts of @a@.
--
-- 'divise' takes the \"splitting\" method and the two sub-consumers, and
-- returns the wrapped/combined consumer.
--
-- All instances of 'Divisible' should be instances of 'Divise' with
-- @'divise' = 'divide'@.
--
-- If a function is polymorphic over @'Divise' f@ (as opposed to @'Divisible'
-- f@), we can provide a stronger guarantee: namely, that any input consumed
-- will be passed to at least one sub-consumer. With @'Divisible' f@, said input
-- could potentially disappear into the void, as this is possible with
-- 'conquer'.
--
-- Mathematically, a functor being an instance of 'Divise' means that it is
-- \"semigroupoidal\" with respect to the contravariant (tupling) Day
-- convolution.  That is, it is possible to define a function @(f `Day` f)
-- a -> f a@ in a way that is associative.
--
-- @since 5.3.6
class Contravariant f => Divise f where
    -- | Takes a \"splitting\" method and the two sub-consumers, and
    -- returns the wrapped/combined consumer.
    divise :: (a -> (b, c)) -> f b -> f c -> f a

-- | Generic 'divise'. Caveats:
--
--   1. Will not compile if @f@ is a sum type.
--   2. Will not compile if @f@ contains fields that do not mention its type variable.
--   3. @-XDeriveGeneric@ is not smart enough to make instances where the type variable appears in negative position.
--
-- @since 5.3.8
gdivise :: (Divise (Rep1 f), Generic1 f) => (a -> (b, c)) -> f b -> f c -> f a
gdivise f x y = to1 $ divise f (from1 x) (from1 y)

-- | Combine a consumer of @a@ with a consumer of @b@ to get a consumer of
-- @(a, b)@.
--
-- @
-- 'divised' = 'divise' 'id'
-- @
--
-- @since 5.3.6
divised :: Divise f => f a -> f b -> f (a, b)
divised = divise id

-- | Generic 'divised'. Caveats are the same as for 'gdivise'.
--
-- @since 5.3.8
gdivised :: (Generic1 f, Divise (Rep1 f)) => f a -> f b -> f (a, b)
gdivised fa fb = gdivise id fa fb

-- | Wrap a 'Divisible' to be used as a member of 'Divise'
--
-- @since 5.3.6
newtype WrappedDivisible f a = WrapDivisible { unwrapDivisible :: f a }

-- | @since 5.3.6
instance Contravariant f => Contravariant (WrappedDivisible f) where
  contramap f (WrapDivisible a) = WrapDivisible (contramap f a)

-- | @since 5.3.6
instance Divisible f => Divise (WrappedDivisible f) where
  divise f (WrapDivisible x) (WrapDivisible y) = WrapDivisible (divide f x y)

-- | Unlike 'Divisible', requires only 'Semigroup' on @r@.
--
-- @since 5.3.6
instance Semigroup r => Divise (Op r) where
    divise f (Op g) (Op h) = Op $ \a -> case f a of
      (b, c) -> g b <> h c

-- | Unlike 'Divisible', requires only 'Semigroup' on @m@.
--
-- @since 5.3.6
instance Semigroup m => Divise (Const m) where
    divise _ (Const a) (Const b) = Const (a <> b)

-- | Unlike 'Divisible', requires only 'Semigroup' on @m@.
--
-- @since 5.3.6
instance Semigroup m => Divise (Constant m) where
    divise _ (Constant a) (Constant b) = Constant (a <> b)

-- | @since 5.3.6
instance Divise Comparison where divise = divide

-- | @since 5.3.6
instance Divise Equivalence where divise = divide

-- | @since 5.3.6
instance Divise Predicate where divise = divide

-- | @since 5.3.6
instance Divise Proxy where divise = divide

#ifdef MIN_VERSION_StateVar
-- | @since 5.3.6
instance Divise SettableStateVar where divise = divide
#endif

-- | @since 5.3.6
instance Divise f => Divise (Alt f) where
  divise f (Alt l) (Alt r) = Alt $ divise f l r

-- | @since 5.3.6
instance Divise U1 where divise = divide

-- | Has no 'Divisible' instance.
--
-- @since 5.3.6
instance Divise V1 where divise _ x = case x of {}

-- | @since 5.3.6
instance Divise f => Divise (Rec1 f) where
  divise f (Rec1 l) (Rec1 r) = Rec1 $ divise f l r

-- | @since 5.3.6
instance Divise f => Divise (M1 i c f) where
  divise f (M1 l) (M1 r) = M1 $ divise f l r

-- | @since 5.3.6
instance (Divise f, Divise g) => Divise (f :*: g) where
  divise f (l1 :*: r1) (l2 :*: r2) = divise f l1 l2 :*: divise f r1 r2

-- | Unlike 'Divisible', requires only 'Apply' on @f@.
--
-- @since 5.3.6
instance (Apply f, Divise g) => Divise (f :.: g) where
  divise f (Comp1 l) (Comp1 r) = Comp1 (liftF2 (divise f) l r)

-- | @since 5.3.6
instance Divise f => Divise (Backwards f) where
  divise f (Backwards l) (Backwards r) = Backwards $ divise f l r

#if !(MIN_VERSION_transformers(0,6,0))
-- | @since 5.3.6
instance Divise m => Divise (ErrorT e m) where
  divise f (ErrorT l) (ErrorT r) = ErrorT $ divise (funzip . fmap f) l r

-- | @since 5.3.6
instance Divise m => Divise (ListT m) where
  divise f (ListT l) (ListT r) = ListT $ divise (funzip . map f) l r
#endif

-- | @since 5.3.6
instance Divise m => Divise (ExceptT e m) where
  divise f (ExceptT l) (ExceptT r) = ExceptT $ divise (funzip . fmap f) l r

-- | @since 5.3.6
instance Divise f => Divise (IdentityT f) where
  divise f (IdentityT l) (IdentityT r) = IdentityT $ divise f l r

-- | @since 5.3.6
instance Divise m => Divise (MaybeT m) where
  divise f (MaybeT l) (MaybeT r) = MaybeT $ divise (funzip . fmap f) l r

-- | @since 5.3.6
instance Divise m => Divise (ReaderT r m) where
  divise abc (ReaderT rmb) (ReaderT rmc) = ReaderT $ \r -> divise abc (rmb r) (rmc r)

-- | @since 5.3.6
instance Divise m => Divise (Lazy.RWST r w s m) where
  divise abc (Lazy.RWST rsmb) (Lazy.RWST rsmc) = Lazy.RWST $ \r s ->
    divise (\ ~(a, s', w) -> case abc a of
                                  ~(b, c) -> ((b, s', w), (c, s', w)))
           (rsmb r s) (rsmc r s)

-- | @since 5.3.6
instance Divise m => Divise (Strict.RWST r w s m) where
  divise abc (Strict.RWST rsmb) (Strict.RWST rsmc) = Strict.RWST $ \r s ->
    divise (\(a, s', w) -> case abc a of
                                (b, c) -> ((b, s', w), (c, s', w)))
           (rsmb r s) (rsmc r s)

-- | @since 5.3.6
instance Divise m => Divise (Lazy.StateT s m) where
  divise f (Lazy.StateT l) (Lazy.StateT r) = Lazy.StateT $ \s ->
    divise (lazyFanout f) (l s) (r s)

-- | @since 5.3.6
instance Divise m => Divise (Strict.StateT s m) where
  divise f (Strict.StateT l) (Strict.StateT r) = Strict.StateT $ \s ->
    divise (strictFanout f) (l s) (r s)

-- | @since 5.3.6
instance Divise m => Divise (Lazy.WriterT w m) where
  divise f (Lazy.WriterT l) (Lazy.WriterT r) = Lazy.WriterT $
    divise (lazyFanout f) l r

-- | @since 5.3.6
instance Divise m => Divise (Strict.WriterT w m) where
  divise f (Strict.WriterT l) (Strict.WriterT r) = Strict.WriterT $
    divise (strictFanout f) l r

-- | Unlike 'Divisible', requires only 'Apply' on @f@.
--
-- @since 5.3.6
instance (Apply f, Divise g) => Divise (Compose f g) where
  divise f (Compose l) (Compose r) = Compose (liftF2 (divise f) l r)

-- | @since 5.3.6
instance (Divise f, Divise g) => Divise (Product f g) where
  divise f (Pair l1 r1) (Pair l2 r2) = Pair (divise f l1 l2) (divise f r1 r2)

-- | @since 5.3.6
instance Divise f => Divise (Reverse f) where
  divise f (Reverse l) (Reverse r) = Reverse $ divise f l r

-- Helpers

lazyFanout :: (a -> (b, c)) -> (a, s) -> ((b, s), (c, s))
lazyFanout f ~(a, s) = case f a of
  ~(b, c) -> ((b, s), (c, s))

strictFanout :: (a -> (b, c)) -> (a, s) -> ((b, s), (c, s))
strictFanout f (a, s) = case f a of
  (b, c) -> ((b, s), (c, s))

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd
