{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators    #-}

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
module Data.Functor.Contravariant.Decide (
    Decide(..)
  , gdecide
  , decided
  , gdecided
  ) where

import Control.Applicative.Backwards
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
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divise
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Monoid (Alt(..))
import Data.Proxy
import GHC.Generics

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Arrow
import Control.Monad.Trans.List
import Data.Either
#endif

#ifdef MIN_VERSION_StateVar
import Data.StateVar
#endif

-- | The contravariant analogue of 'Alt'.
--
-- If one thinks of @f a@ as a consumer of @a@s, then 'decide' allows one
-- to handle the consumption of a value by choosing to handle it via
-- exactly one of two independent consumers.  It redirects the input
-- completely into one of two consumers.
--
-- 'decide' takes the \"decision\" method and the two potential consumers,
-- and returns the wrapped/combined consumer.
--
-- Mathematically, a functor being an instance of 'Decide' means that it is
-- \"semigroupoidal\" with respect to the contravariant \"either-based\" Day
-- convolution (@data EitherDay f g a = forall b c. EitherDay (f b) (g c) (a -> Either b c)@).
-- That is, it is possible to define a function @(f `EitherDay` f) a ->
-- f a@ in a way that is associative.
--
-- @since 5.3.6
class Contravariant f => Decide f where
    -- | Takes the \"decision\" method and the two potential consumers, and
    -- returns the wrapped/combined consumer.
    decide :: (a -> Either b c) -> f b -> f c -> f a

-- | Generic 'decide'. Caveats:
--
--   1. Will not compile if @f@ is a sum type.
--   2. Will not compile if @f@ contains fields that do not mention its type variable.
--   3. @-XDeriveGeneric@ is not smart enough to make instances where the type variable appears in negative position.
--
-- @since 5.3.8
gdecide :: (Generic1 f, Decide (Rep1 f)) => (a -> Either b c) -> f b -> f c -> f a
gdecide f fb fc = to1 $ decide f (from1 fb) (from1 fc)

-- | For @'decided' x y@, the resulting @f ('Either' b c)@ will direct
-- 'Left's to be consumed by @x@, and 'Right's to be consumed by y.
--
-- @since 5.3.6
decided :: Decide f => f b -> f c -> f (Either b c)
decided = decide id

-- | Generic 'decided'. Caveats are the same as for 'gdecide'.
--
-- @since 5.3.8
gdecided :: (Generic1 f, Decide (Rep1 f)) => f b -> f c -> f (Either b c)
gdecided fb fc = gdecide id fb fc

-- | @since 5.3.6
instance Decidable f => Decide (WrappedDivisible f) where
    decide f (WrapDivisible x) (WrapDivisible y) = WrapDivisible (choose f x y)

-- | @since 5.3.6
instance Decide Comparison where decide = choose

-- | @since 5.3.6
instance Decide Equivalence where decide = choose

-- | @since 5.3.6
instance Decide Predicate where decide = choose

-- | Unlike 'Decidable', requires no constraint on @r@.
--
-- @since 5.3.6
instance Decide (Op r) where
  decide f (Op g) (Op h) = Op $ either g h . f

-- | @since 5.3.6
instance Decide f => Decide (Alt f) where
  decide f (Alt l) (Alt r) = Alt $ decide f l r

-- | @since 5.3.6
instance Decide U1 where decide = choose

-- | Has no 'Decidable' or 'Conclude' instance.
--
-- @since 5.3.6
instance Decide V1 where decide _ x = case x of {}

-- | @since 5.3.6
instance Decide f => Decide (Rec1 f) where
  decide f (Rec1 l) (Rec1 r) = Rec1 $ decide f l r

-- | @since 5.3.6
instance Decide f => Decide (M1 i c f) where
  decide f (M1 l) (M1 r) = M1 $ decide f l r

-- | @since 5.3.6
instance (Decide f, Decide g) => Decide (f :*: g) where
  decide f (l1 :*: r1) (l2 :*: r2) = decide f l1 l2 :*: decide f r1 r2

-- | Unlike 'Decidable', requires only 'Apply' on @f@.
--
-- @since 5.3.6
instance (Apply f, Decide g) => Decide (f :.: g) where
  decide f (Comp1 l) (Comp1 r) = Comp1 (liftF2 (decide f) l r)

-- | @since 5.3.6
instance Decide f => Decide (Backwards f) where
  decide f (Backwards l) (Backwards r) = Backwards $ decide f l r

-- | @since 5.3.6
instance Decide f => Decide (IdentityT f) where
  decide f (IdentityT l) (IdentityT r) = IdentityT $ decide f l r

-- | @since 5.3.6
instance Decide m => Decide (ReaderT r m) where
  decide abc (ReaderT rmb) (ReaderT rmc) = ReaderT $ \r -> decide abc (rmb r) (rmc r)

-- | @since 5.3.6
instance Decide m => Decide (Lazy.RWST r w s m) where
  decide abc (Lazy.RWST rsmb) (Lazy.RWST rsmc) = Lazy.RWST $ \r s ->
    decide (\ ~(a, s', w) -> either (Left  . betuple3 s' w)
                                    (Right . betuple3 s' w)
                                    (abc a))
           (rsmb r s) (rsmc r s)

-- | @since 5.3.6
instance Decide m => Decide (Strict.RWST r w s m) where
  decide abc (Strict.RWST rsmb) (Strict.RWST rsmc) = Strict.RWST $ \r s ->
    decide (\(a, s', w) -> either (Left  . betuple3 s' w)
                                  (Right . betuple3 s' w)
                                  (abc a))
           (rsmb r s) (rsmc r s)

#if !(MIN_VERSION_transformers(0,6,0))
-- | @since 5.3.6
instance Divise m => Decide (ListT m) where
  decide f (ListT l) (ListT r) = ListT $ divise ((lefts &&& rights) . map f) l r
#endif

-- | @since 5.3.6
instance Divise m => Decide (MaybeT m) where
  decide f (MaybeT l) (MaybeT r) = MaybeT $
    divise ( maybe (Nothing, Nothing)
                   (either (\b -> (Just b, Nothing))
                           (\c -> (Nothing, Just c)) . f)
           ) l r

-- | @since 5.3.6
instance Decide m => Decide (Lazy.StateT s m) where
  decide f (Lazy.StateT l) (Lazy.StateT r) = Lazy.StateT $ \s ->
    decide (\ ~(a, s') -> either (Left . betuple s') (Right . betuple s') (f a))
           (l s) (r s)

-- | @since 5.3.6
instance Decide m => Decide (Strict.StateT s m) where
  decide f (Strict.StateT l) (Strict.StateT r) = Strict.StateT $ \s ->
    decide (\(a, s') -> either (Left . betuple s') (Right . betuple s') (f a))
           (l s) (r s)

-- | @since 5.3.6
instance Decide m => Decide (Lazy.WriterT w m) where
  decide f (Lazy.WriterT l) (Lazy.WriterT r) = Lazy.WriterT $
    decide (\ ~(a, s') -> either (Left . betuple s') (Right . betuple s') (f a)) l r

-- | @since 5.3.6
instance Decide m => Decide (Strict.WriterT w m) where
  decide f (Strict.WriterT l) (Strict.WriterT r) = Strict.WriterT $
    decide (\(a, s') -> either (Left . betuple s') (Right . betuple s') (f a)) l r

-- | Unlike 'Decidable', requires only 'Apply' on @f@.
--
-- @since 5.3.6
instance (Apply f, Decide g) => Decide (Compose f g) where
  decide f (Compose l) (Compose r) = Compose (liftF2 (decide f) l r)

-- | @since 5.3.6
instance (Decide f, Decide g) => Decide (Product f g) where
  decide f (Pair l1 r1) (Pair l2 r2) = Pair (decide f l1 l2) (decide f r1 r2)

-- | @since 5.3.6
instance Decide f => Decide (Reverse f) where
  decide f (Reverse l) (Reverse r) = Reverse $ decide f l r

betuple :: s -> a -> (a, s)
betuple s a = (a, s)

betuple3 :: s -> w -> a -> (a, s, w)
betuple3 s w a = (a, s, w)

-- | @since 5.3.6
instance Decide Proxy where
  decide _ Proxy Proxy = Proxy

#ifdef MIN_VERSION_StateVar
-- | @since 5.3.6
instance Decide SettableStateVar where
  decide k (SettableStateVar l) (SettableStateVar r) = SettableStateVar $ \ a -> case k a of
    Left b -> l b
    Right c -> r c
#endif
