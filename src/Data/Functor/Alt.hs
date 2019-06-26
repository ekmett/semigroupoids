{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE ConstrainedClassMethods #-}
#endif
{-# options_ghc -fno-warn-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Alt
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Alt
  ( Alt(..)
  , optional
  , module Data.Functor.Apply
  ) where

import Control.Applicative hiding (some, many, optional)
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Arrow
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Trans.Identity
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
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Reverse
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Semigroup (Option(..), Semigroup(..))
import qualified Data.Semigroup as Semigroup
import Prelude (($),Either(..),Maybe(..),const,IO,Ord,(++),(.),either,seq,undefined)
import Unsafe.Coerce

#ifdef MIN_VERSION_containers
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
# if MIN_VERSION_base(4,8,0)
import Prelude (mappend)
# else
import Data.Monoid (mappend)
# endif
#endif

#if defined(MIN_VERSION_tagged) || (MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif

#ifdef MIN_VERSION_unordered_containers
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Prelude (Eq)
#endif

#ifdef MIN_VERSION_generic_deriving
import Generics.Deriving.Base
#else
import GHC.Generics
#endif


infixl 3 <!>

-- | Laws:
--
-- > <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
-- > <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
--
-- If extended to an 'Alternative' then '<!>' should equal '<|>'.
--
-- Ideally, an instance of 'Alt' also satisfies the \"left distributon\" law of
-- MonadPlus with respect to '<.>':
--
-- > <.> right-distributes over <!>: (a <!> b) <.> c = (a <.> c) <!> (b <.> c)
--
-- But 'Maybe', 'IO', @'Either' a@, @'ErrorT' e m@, and 'STM' satisfy the alternative
-- \"left catch\" law instead:
--
-- > pure a <!> b = pure a
--
-- However, this variation cannot be stated purely in terms of the dependencies of 'Alt'.
--
-- When and if MonadPlus is successfully refactored, this class should also
-- be refactored to remove these instances.
--
-- The right distributive law should extend in the cases where the a 'Bind' or 'Monad' is
-- provided to yield variations of the right distributive law:
--
-- > (m <!> n) >>- f = (m >>- f) <!> (m >>- f)
-- > (m <!> n) >>= f = (m >>= f) <!> (m >>= f)

class Functor f => Alt f where
  -- | '<|>' without a required @empty@
  (<!>) :: f a -> f a -> f a

  some :: Applicative f => f a -> f [a]
  some v = some_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

  many :: Applicative f => f a -> f [a]
  many v = many_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

-- | One or none.
optional :: (Alt f, Applicative f) => f a -> f (Maybe a)
optional v = Just <$> v <!> pure Nothing

instance (Alt f, Alt g) => Alt (f :*: g) where
  (as :*: bs) <!> (cs :*: ds) = (as <!> cs) :*: (bs <!> ds)

newtype Magic f = Magic { runMagic :: forall a. Applicative f => f a -> f [a] }

instance Alt f => Alt (M1 i c f) where
  M1 f <!> M1 g = M1 (f <!> g)
  some = runMagic (unsafeCoerce (Magic some :: Magic f))
  many = runMagic (unsafeCoerce (Magic many :: Magic f))

instance Alt f => Alt (Rec1 f) where
  Rec1 f <!> Rec1 g = Rec1 (f <!> g)
  some = runMagic (unsafeCoerce (Magic some :: Magic f))
  many = runMagic (unsafeCoerce (Magic many :: Magic f))

instance Alt U1 where
  _ <!> _ = U1
  some _ = U1
  many _ = U1

instance Alt V1 where
  v <!> u = v `seq` u `seq` undefined
  some v = v `seq` undefined
  many v = v `seq` undefined

#if defined(MIN_VERSION_tagged) || (MIN_VERSION_base(4,7,0))
instance Alt Proxy where
  _ <!> _ = Proxy
  some _ = Proxy
  many _ = Proxy
#endif

instance Alt (Either a) where
  Left _ <!> b = b
  a      <!> _ = a

-- | This instance does not actually satisfy the ('<.>') right distributive law
-- It instead satisfies the "Left-Catch" law
instance Alt IO where
  m <!> n = catch m (go n) where
    go :: x -> SomeException -> x
    go = const

instance Alt [] where
  (<!>) = (++)

instance Alt Maybe where
  Nothing <!> b = b
  a       <!> _ = a

instance Alt Option where
  (<!>) = (<|>)

instance MonadPlus m => Alt (WrappedMonad m) where
  (<!>) = (<|>)

instance ArrowPlus a => Alt (WrappedArrow a b) where
  (<!>) = (<|>)

#ifdef MIN_VERSION_containers
instance Ord k => Alt (Map k) where
  (<!>) = Map.union

instance Alt IntMap where
  (<!>) = IntMap.union

instance Alt Seq where
  (<!>) = mappend
#endif

#ifdef MIN_VERSION_unordered_containers
instance (Hashable k, Eq k) => Alt (HashMap k) where
  (<!>) = HashMap.union
#endif

instance Alt NonEmpty where
  (a :| as) <!> ~(b :| bs) = a :| (as ++ b : bs)

instance Alternative f => Alt (WrappedApplicative f) where
  WrapApplicative a <!> WrapApplicative b = WrapApplicative (a <|> b)

instance Alt f => Alt (IdentityT f) where
  IdentityT a <!> IdentityT b = IdentityT (a <!> b)

instance Alt f => Alt (ReaderT e f) where
  ReaderT a <!> ReaderT b = ReaderT $ \e -> a e <!> b e

instance (Bind f, Monad f) => Alt (MaybeT f) where
  MaybeT a <!> MaybeT b = MaybeT $ do
    v <- a
    case v of
      Nothing -> b
      Just _ -> return v

instance (Bind f, Monad f) => Alt (ErrorT e f) where
  ErrorT m <!> ErrorT n = ErrorT $ do
    a <- m
    case a of
      Left _ -> n
      Right r -> return (Right r)

instance (Bind f, Monad f, Semigroup e) => Alt (ExceptT e f) where
  ExceptT m <!> ExceptT n = ExceptT $ do
    a <- m
    case a of
      Left e -> liftM (either (Left . (<>) e) Right) n
      Right x -> return (Right x)

instance Apply f => Alt (ListT f) where
  ListT a <!> ListT b = ListT $ (<!>) <$> a <.> b

instance Alt f => Alt (Strict.StateT e f) where
  Strict.StateT m <!> Strict.StateT n = Strict.StateT $ \s -> m s <!> n s

instance Alt f => Alt (Lazy.StateT e f) where
  Lazy.StateT m <!> Lazy.StateT n = Lazy.StateT $ \s -> m s <!> n s

instance Alt f => Alt (Strict.WriterT w f) where
  Strict.WriterT m <!> Strict.WriterT n = Strict.WriterT $ m <!> n

instance Alt f => Alt (Lazy.WriterT w f) where
  Lazy.WriterT m <!> Lazy.WriterT n = Lazy.WriterT $ m <!> n

instance Alt f => Alt (Strict.RWST r w s f) where
  Strict.RWST m <!> Strict.RWST n = Strict.RWST $ \r s -> m r s <!> n r s

instance Alt f => Alt (Lazy.RWST r w s f) where
  Lazy.RWST m <!> Lazy.RWST n = Lazy.RWST $ \r s -> m r s <!> n r s

instance Alt f => Alt (Backwards f) where
  Backwards a <!> Backwards b = Backwards (a <!> b)

instance (Alt f, Functor g) => Alt (Compose f g) where
  Compose a <!> Compose b = Compose (a <!> b)

instance Alt f => Alt (Lift f) where
  Pure a   <!> _       = Pure a
  Other _  <!> Pure b  = Pure b
  Other a  <!> Other b = Other (a <!> b)

instance (Alt f, Alt g) => Alt (Product f g) where
  Pair a1 b1 <!> Pair a2 b2 = Pair (a1 <!> a2) (b1 <!> b2)

instance Alt f => Alt (Reverse f) where
  Reverse a <!> Reverse b = Reverse (a <!> b)

instance Alt Semigroup.First where
  (<!>) = (<>)

instance Alt Semigroup.Last where
  (<!>) = (<>)

instance Alt Monoid.First where
  (<!>) = mappend

instance Alt Monoid.Last where
  (<!>) = mappend
