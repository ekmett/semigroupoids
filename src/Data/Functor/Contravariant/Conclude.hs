{-# LANGUAGE CPP           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}

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
module Data.Functor.Contravariant.Conclude (
    Conclude(..)
  , gconclude
  , concluded
  , gconcluded
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
import Data.Functor.Contravariant.Decide
import Data.Functor.Contravariant.Divise
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Monoid (Alt(..))
import Data.Proxy
import Data.Void
import GHC.Generics

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.List
#endif

#ifdef MIN_VERSION_StateVar
import Data.StateVar
#endif

-- | The contravariant analogue of 'Plus'.  Adds on to 'Decide' the ability
-- to express a combinator that rejects all input, to act as the dead-end.
-- Essentially 'Decidable' without a superclass constraint on 'Divisible'.
--
-- If one thinks of @f a@ as a consumer of @a@s, then 'conclude' defines
-- a consumer that cannot ever receive /any/ input.
--
-- Conclude acts as an identity with 'decide', because any decision that
-- involves 'conclude' must necessarily /always/ pick the other option.
--
-- That is, for, say,
--
-- @
-- 'decide' f x 'concluded'
-- @
--
-- @f@ is the deciding function that picks which of the inputs of @decide@
-- to direct input to; in the situation above, @f@ must /always/ direct all
-- input to @x@, and never 'concluded'.
--
-- Mathematically, a functor being an instance of 'Decide' means that it is
-- \"monoidal\" with respect to the contravariant "either-based" Day
-- convolution described in the documentation of 'Decide'.  On top of
-- 'Decide', it adds a way to construct an \"identity\" @conclude@ where
-- @decide f x (conclude q) == x@, and @decide g (conclude r) y == y@.
--
-- @since 5.3.6
class Decide f => Conclude f where
    -- | The consumer that cannot ever receive /any/ input.
    conclude :: (a -> Void) -> f a

-- | Generic 'conclude'. Caveats:
--
--   1. Will not compile if @f@ is a sum type.
--   2. Will not compile if @f@ contains fields that do not mention its type variable.
--
-- @since 5.3.8
gconclude :: (Generic1 f, Conclude (Rep1 f)) => (a -> Void) -> f a
gconclude f = to1 $ conclude f

-- | A potentially more meaningful form of 'conclude', the consumer that cannot
-- ever receive /any/ input.  That is because it expects only input of type
-- 'Void', but such a type has no values.
--
-- @
-- 'concluded' = 'conclude' 'id'
-- @
--
-- @since 5.3.6
concluded :: Conclude f => f Void
concluded = conclude id

-- | Generic 'concluded'. Caveats are the same as for 'gconclude'.
--
-- @since 5.3.8
gconcluded :: (Generic1 f, Conclude (Rep1 f)) => f Void
gconcluded = to1 concluded

-- | @since 5.3.6
instance Decidable f => Conclude (WrappedDivisible f) where
    conclude f = WrapDivisible (lose f)

-- | @since 5.3.6
instance Conclude Comparison where conclude = lose

-- | @since 5.3.6
instance Conclude Equivalence where conclude = lose

-- | @since 5.3.6
instance Conclude Predicate where conclude = lose

-- | @since 5.3.6
instance Conclude (Op r) where
  conclude f = Op $ absurd . f

-- | @since 5.3.6
instance Conclude Proxy where conclude = lose

#ifdef MIN_VERSION_StateVar
-- | @since 5.3.6
instance Conclude SettableStateVar where conclude = lose
#endif

-- | @since 5.3.6
instance Conclude f => Conclude (Alt f) where
  conclude = Alt . conclude

-- | @since 5.3.6
instance Conclude U1 where conclude = lose

-- | @since 5.3.6
instance Conclude f => Conclude (Rec1 f) where
  conclude = Rec1 . conclude

-- | @since 5.3.6
instance Conclude f => Conclude (M1 i c f) where
  conclude = M1 . conclude

-- | @since 5.3.6
instance (Conclude f, Conclude g) => Conclude (f :*: g) where
  conclude f = conclude f :*: conclude f

-- | @since 5.3.6
instance (Apply f, Applicative f, Conclude g) => Conclude (f :.: g) where
  conclude = Comp1 . pure . conclude

-- | @since 5.3.6
instance Conclude f => Conclude (Backwards f) where
  conclude = Backwards . conclude

-- | @since 5.3.6
instance Conclude f => Conclude (IdentityT f) where
  conclude = IdentityT . conclude

-- | @since 5.3.6
instance Conclude m => Conclude (ReaderT r m) where
  conclude f = ReaderT $ \_ -> conclude f

-- | @since 5.3.6
instance Conclude m => Conclude (Lazy.RWST r w s m) where
  conclude f = Lazy.RWST $ \_ _ -> contramap (\ ~(a, _, _) -> a) (conclude f)

-- | @since 5.3.6
instance Conclude m => Conclude (Strict.RWST r w s m) where
  conclude f = Strict.RWST $ \_ _ -> contramap (\(a, _, _) -> a) (conclude f)

#if !(MIN_VERSION_transformers(0,6,0))
-- | @since 5.3.6
instance (Divisible m, Divise m) => Conclude (ListT m) where
  conclude _ = ListT conquer
#endif

-- | @since 5.3.6
instance (Divisible m, Divise m) => Conclude (MaybeT m) where
  conclude _ = MaybeT conquer

-- | @since 5.3.6
instance Conclude m => Conclude (Lazy.StateT s m) where
  conclude f = Lazy.StateT $ \_ -> contramap lazyFst (conclude f)

-- | @since 5.3.6
instance Conclude m => Conclude (Strict.StateT s m) where
  conclude f = Strict.StateT $ \_ -> contramap fst (conclude f)

-- | @since 5.3.6
instance Conclude m => Conclude (Lazy.WriterT w m) where
  conclude f = Lazy.WriterT $ contramap lazyFst (conclude f)

-- | @since 5.3.6
instance Conclude m => Conclude (Strict.WriterT w m) where
  conclude f = Strict.WriterT $ contramap fst (conclude f)

-- | @since 5.3.6
instance (Apply f, Applicative f, Conclude g) => Conclude (Compose f g) where
  conclude = Compose . pure . conclude

-- | @since 5.3.6
instance (Conclude f, Conclude g) => Conclude (Product f g) where
  conclude f = Pair (conclude f) (conclude f)

-- | @since 5.3.6
instance Conclude f => Conclude (Reverse f) where
  conclude = Reverse . conclude

-- Helpers

lazyFst :: (a, b) -> a
lazyFst ~(a, _) = a
