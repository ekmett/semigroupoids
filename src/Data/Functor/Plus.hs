{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
  , module Data.Functor.Alt
  ) where

import Control.Applicative hiding (some, many)
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Arrow
-- import Control.Exception
import Control.Monad
import Control.Monad.Trans.Identity
-- import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Reverse
import qualified Data.Monoid as Monoid
import Data.Semigroup hiding (Product)
import Prelude hiding (id, (.))

#ifdef MIN_VERSION_containers
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
#endif

#if defined(MIN_VERSION_tagged) || (MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif

#ifdef MIN_VERSION_unordered_containers
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
#endif

#ifdef MIN_VERSION_generic_deriving
import Generics.Deriving.Base
#else
import GHC.Generics
#endif

-- | Laws:
--
-- > zero <!> m = m
-- > m <!> zero = m
--
-- If extended to an 'Alternative' then 'zero' should equal 'empty'.

class Alt f => Plus f where
  zero :: f a

instance Plus Proxy where
  zero = Proxy

instance Plus U1 where
  zero = U1

instance (Plus f, Plus g) => Plus (f :*: g) where
  zero = zero :*: zero

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

instance Plus Option where
  zero = empty

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

instance (Bind f, Monad f) => Plus (MaybeT f) where
  zero = MaybeT $ return zero

instance (Bind f, Monad f, Error e) => Plus (ErrorT e f) where
  zero = ErrorT $ return $ Left noMsg

instance (Bind f, Monad f, Semigroup e, Monoid e) => Plus (ExceptT e f) where
  zero = ExceptT $ return $ Left mempty

instance (Apply f, Applicative f) => Plus (ListT f) where
  zero = ListT $ pure []

instance Plus f => Plus (Strict.StateT e f) where
  zero = Strict.StateT $ \_ -> zero

instance Plus f => Plus (Lazy.StateT e f) where
  zero = Lazy.StateT $ \_ -> zero

instance Plus f => Plus (Strict.WriterT w f) where
  zero = Strict.WriterT zero

instance Plus f => Plus (Lazy.WriterT w f) where
  zero = Lazy.WriterT zero

instance Plus f => Plus (Strict.RWST r w s f) where
  zero = Strict.RWST $ \_ _ -> zero

instance Plus f => Plus (Lazy.RWST r w s f) where
  zero = Lazy.RWST $ \_ _ -> zero

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
