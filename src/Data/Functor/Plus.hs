{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Plus
  ( Plus(..)
  , psum
  , gzero
  , module Data.Functor.Alt
  ) where

import Control.Applicative hiding (some, many)
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Semigroupoids.Internal
#endif
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Data.Foldable hiding (asum)
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Reverse
import qualified Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup hiding (Product)
import GHC.Generics
import Prelude hiding (id, (.), foldr)

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
#endif

#ifdef MIN_VERSION_containers
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
#endif

#ifdef MIN_VERSION_unordered_containers
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
#endif

-- | Laws:
--
-- > zero <!> m = m
-- > m <!> zero = m
--
-- If extended to an 'Alternative' then 'zero' should equal 'empty'.
class Alt f => Plus f where
  zero :: f a

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- >>> psum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"
--
-- @since 5.3.6
psum :: (Foldable t, Plus f) => t (f a) -> f a
psum = foldr (<!>) zero

-- | Generic 'zero'. Caveats:
--
--   1. Will not compile if @f@ is a sum type.
--   2. Any types where the @a@ does not appear must have a 'Monoid' instance.
--
-- @since 5.3.8
gzero :: (Plus (Rep1 f), Generic1 f) => f a
gzero = to1 zero

instance Plus Proxy where
  zero = Proxy

instance Plus U1 where
  zero = U1

-- | @since 5.3.8
instance (Monoid c
#if !(MIN_VERSION_base(4,11,0))
         , Semigroup c
#endif
  ) => Plus (K1 i c) where
  zero = K1 mempty

instance (Plus f, Plus g) => Plus (f :*: g) where
  zero = zero :*: zero

-- | @since 5.3.8
instance (Plus f, Functor g) => Plus (f :.: g) where
  zero = Comp1 zero

instance Plus f => Plus (M1 i c f) where
  zero = M1 zero

instance Plus f => Plus (Rec1 f) where
  zero = Rec1 zero

instance Plus IO where
  zero = error "zero"

instance Plus [] where
  zero = []

instance Plus Maybe where
  zero = Nothing

#if !(MIN_VERSION_base(4,16,0))
instance Plus Option where
  zero = empty
#endif

instance MonadPlus m => Plus (WrappedMonad m) where
  zero = empty

instance ArrowPlus a => Plus (WrappedArrow a b) where
  zero = empty

#ifdef MIN_VERSION_containers
instance Ord k => Plus (Map k) where
  zero = Map.empty

instance Plus IntMap where
  zero = IntMap.empty

instance Plus Seq where
  zero = mempty
#endif

#ifdef MIN_VERSION_unordered_containers
instance (Hashable k, Eq k) => Plus (HashMap k) where
  zero = HashMap.empty
#endif

instance Alternative f => Plus (WrappedApplicative f) where
  zero = empty

instance Plus f => Plus (IdentityT f) where
  zero = IdentityT zero

instance Plus f => Plus (ReaderT e f) where
  zero = ReaderT $ \_ -> zero

instance (Functor f, Monad f) => Plus (MaybeT f) where
  zero = MaybeT $ return zero

#if !(MIN_VERSION_transformers(0,6,0))
instance (Functor f, Monad f, Error e) => Plus (ErrorT e f) where
  zero = ErrorT $ return $ Left noMsg

instance (Apply f, Applicative f) => Plus (ListT f) where
  zero = ListT $ pure []
#endif

instance (Functor f, Monad f, Semigroup e, Monoid e) => Plus (ExceptT e f) where
  zero = ExceptT $ return $ Left mempty

instance Plus f => Plus (Strict.StateT e f) where
  zero = Strict.StateT $ \_ -> zero

instance Plus f => Plus (Lazy.StateT e f) where
  zero = Lazy.StateT $ \_ -> zero

instance Plus f => Plus (Strict.WriterT w f) where
  zero = Strict.WriterT zero

instance Plus f => Plus (Lazy.WriterT w f) where
  zero = Lazy.WriterT zero

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Plus f) => Plus (CPS.WriterT w f) where
  zero = mkWriterT $ const zero
#endif

instance Plus f => Plus (Strict.RWST r w s f) where
  zero = Strict.RWST $ \_ _ -> zero

instance Plus f => Plus (Lazy.RWST r w s f) where
  zero = Lazy.RWST $ \_ _ -> zero

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Plus f) => Plus (CPS.RWST r w s f) where
  zero = mkRWST $ \_ _ _ -> zero
#endif

instance Plus f => Plus (Backwards f) where
  zero = Backwards zero

instance (Plus f, Functor g) => Plus (Compose f g) where
  zero = Compose zero

instance Plus f => Plus (Lift f) where
  zero = Other zero

instance (Plus f, Plus g) => Plus (Product f g) where
  zero = Pair zero zero

instance Plus f => Plus (Reverse f) where
  zero = Reverse zero

instance Plus Monoid.First where
  zero = Monoid.First Nothing

instance Plus Monoid.Last where
  zero = Monoid.Last Nothing
