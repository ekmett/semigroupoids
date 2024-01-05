{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2018 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is used to resolve the cyclic we get from defining these
-- classes here rather than in a package upstream. Otherwise we'd get
-- orphaned heads for many instances on the types in @transformers@ and @bifunctors@.
----------------------------------------------------------------------------
module Data.Functor.Bind.Class (
  -- * Applyable functors
    Apply(..)
  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  , (<.*>)
  , (<*.>)
  , traverse1Maybe
  -- * Bindable functors
  , Bind(..)
  , apDefault
  , returning
  -- * Biappliable bifunctors
  , Biapply(..)
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Arrow
import Control.Category
import Control.Monad (ap)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Semigroupoids.Internal
#endif
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifunctor.Biff
import Data.Bifunctor.Clown
import Data.Bifunctor.Flip
import Data.Bifunctor.Joker
import Data.Bifunctor.Join
import Data.Bifunctor.Product as Bifunctor
import Data.Bifunctor.Tannen
import Data.Bifunctor.Wrapped
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Functor.Extend
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down (..))
import Data.Proxy
import Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid
import Data.Orphans ()
import GHC.Generics as Generics
import Language.Haskell.TH (Q)
import Prelude hiding (id, (.))

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
#endif

#ifdef MIN_VERSION_containers
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Tree (Tree)
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#ifdef MIN_VERSION_unordered_containers
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
#endif

#ifdef MIN_VERSION_comonad
import Control.Comonad
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced
#else
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

infixl 1 >>-
infixl 4 <.>, <., .>

-- | A strong lax semi-monoidal endofunctor.
-- This is equivalent to an 'Applicative' without 'pure'.
--
-- Laws:
--
-- @
-- ('.') '<$>' u '<.>' v '<.>' w = u '<.>' (v '<.>' w)
-- x '<.>' (f '<$>' y) = ('.' f) '<$>' x '<.>' y
-- f '<$>' (x '<.>' y) = (f '.') '<$>' x '<.>' y
-- @
--
-- The laws imply that `.>` and `<.` really ignore their
-- left and right results, respectively, and really
-- return their right and left results, respectively.
-- Specifically,
--
-- @
-- (mf '<$>' m) '.>' (nf '<$>' n) = nf '<$>' (m '.>' n)
-- (mf '<$>' m) '<.' (nf '<$>' n) = mf '<$>' (m '<.' n)
-- @
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b
  (<.>) = liftF2 id

  -- | @ a '.>' b = 'const' 'id' '<$>' a '<.>' b @
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | @ a '<.' b = 'const' '<$>' a '<.>' b @
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

  -- | Lift a binary function into a comonad with zipping
  liftF2 :: (a -> b -> c) -> f a -> f b -> f c
  liftF2 f a b = f <$> a <.> b
  {-# INLINE liftF2 #-}

  {-# MINIMAL (<.>) | liftF2 #-}

#ifdef MIN_VERSION_tagged
instance Apply (Tagged a) where
  (<.>) = (<*>)
  (<.) = (<*)
  (.>) = (*>)
#endif

instance Apply Proxy where
  (<.>) = (<*>)
  (<.) = (<*)
  (.>) = (*>)

instance Apply f => Apply (Backwards f) where
  Backwards f <.> Backwards a = Backwards (flip id <$> a <.> f)

instance (Apply f, Apply g) => Apply (Compose f g) where
  Compose f <.> Compose x = Compose ((<.>) <$> f <.> x)

-- | A @'Constant' f@ is not 'Applicative' unless its @f@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup f => Apply (Constant f) where
  Constant a <.> Constant b = Constant (a <> b)
  Constant a <.  Constant b = Constant (a <> b)
  Constant a  .> Constant b = Constant (a <> b)

instance Apply f => Apply (Lift f) where
  Pure f  <.> Pure x  = Pure (f x)
  Pure f  <.> Other y = Other (f <$> y)
  Other f <.> Pure x  = Other (($ x) <$> f)
  Other f <.> Other y = Other (f <.> y)

instance (Apply f, Apply g) => Apply (Functor.Product f g) where
  Functor.Pair f g <.> Functor.Pair x y = Functor.Pair (f <.> x) (g <.> y)

instance Apply f => Apply (Reverse f) where
  Reverse a <.> Reverse b = Reverse (a <.> b)

-- | A @'(,)' m@ is not 'Applicative' unless its @m@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup m => Apply ((,)m) where
  (m, f) <.> (n, a) = (m <> n, f a)
  (m, a) <.  (n, _) = (m <> n, a)
  (m, _)  .> (n, b) = (m <> n, b)

instance Apply NonEmpty where
  (<.>) = ap

instance Apply (Either a) where
  Left a  <.> _       = Left a
  Right _ <.> Left a  = Left a
  Right f <.> Right b = Right (f b)

  Left a  <.  _       = Left a
  Right _ <.  Left a  = Left a
  Right a <.  Right _ = Right a

  Left a   .> _       = Left a
  Right _  .> Left a  = Left a
  Right _  .> Right b = Right b

-- | A @'Const' m@ is not 'Applicative' unless its @m@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup m => Apply (Const m) where
  Const m <.> Const n = Const (m <> n)
  Const m <.  Const n = Const (m <> n)
  Const m  .> Const n = Const (m <> n)

instance Apply ((->)m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply ZipList where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply [] where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply IO where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply Maybe where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

#if !(MIN_VERSION_base(4,16,0))
instance Apply Option where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)
#endif

instance Apply Identity where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply w => Apply (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

instance Monad m => Apply (WrappedMonad m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Arrow a => Apply (WrappedArrow a b) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply Complex where
  (a :+ b) <.> (c :+ d) = a c :+ b d

-- Applicative Q was only added in template-haskell 2.7 (GHC 7.4), so
-- define in terms of Monad instead.
instance Apply Q where
  (<.>) = ap

#ifdef MIN_VERSION_containers
-- | A 'Map k' is not 'Applicative', but it is an instance of 'Apply'
instance Ord k => Apply (Map k) where
  (<.>) = Map.intersectionWith id
  (<. ) = Map.intersectionWith const
  ( .>) = Map.intersectionWith (const id)

-- | An 'IntMap' is not 'Applicative', but it is an instance of 'Apply'
instance Apply IntMap where
  (<.>) = IntMap.intersectionWith id
  (<. ) = IntMap.intersectionWith const
  ( .>) = IntMap.intersectionWith (const id)

instance Apply Seq where
  (<.>) = ap

instance Apply Tree where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)
#endif

#ifdef MIN_VERSION_unordered_containers
-- | A 'HashMap k' is not 'Applicative', but it is an instance of 'Apply'
instance (Hashable k, Eq k) => Apply (HashMap k) where
  (<.>) = HashMap.intersectionWith id
#endif

-- MaybeT is _not_ the same as Compose f Maybe
instance (Functor m, Monad m) => Apply (MaybeT m) where
  (<.>) = apDefault

#if !(MIN_VERSION_transformers(0,6,0))
-- ErrorT e is _not_ the same as Compose f (Either e)
instance (Functor m, Monad m) => Apply (ErrorT e m) where
  (<.>) = apDefault

instance Apply m => Apply (ListT m) where
  ListT f <.> ListT a = ListT $ (<.>) <$> f <.> a
#endif

instance (Functor m, Monad m) => Apply (ExceptT e m) where
  (<.>) = apDefault

instance Apply m => Apply (ReaderT e m) where
  ReaderT f <.> ReaderT a = ReaderT $ \e -> f e <.> a e

-- unfortunately, WriterT has its wrapped product in the wrong order to just use (<.>) instead of flap
-- | A @'Strict.WriterT' w m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Apply m, Semigroup w) => Apply (Strict.WriterT w m) where
  Strict.WriterT f <.> Strict.WriterT a = Strict.WriterT $ flap <$> f <.> a where
    flap (x,m) (y,n) = (x y, m <> n)

-- | A @'Lazy.WriterT' w m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Apply m, Semigroup w) => Apply (Lazy.WriterT w m) where
  Lazy.WriterT f <.> Lazy.WriterT a = Lazy.WriterT $ flap <$> f <.> a where
    flap ~(x,m) ~(y,n) = (x y, m <> n)

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Bind m) => Apply (CPS.WriterT w m) where
  mf <.> mx = mkWriterT $ \w ->
    unWriterT mf w >>- \(f, w') -> unWriterT (f <$> mx) w'
#endif

instance Bind m => Apply (Strict.StateT s m) where
  (<.>) = apDefault

instance Bind m => Apply (Lazy.StateT s m) where
  (<.>) = apDefault

-- | An @'Strict.RWST' r w s m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Bind m, Semigroup w) => Apply (Strict.RWST r w s m) where
  (<.>) = apDefault

-- | An @'Lazy.RWST' r w s m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Bind m, Semigroup w) => Apply (Lazy.RWST r w s m) where
  (<.>) = apDefault

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Bind m) => Apply (CPS.RWST r w s m) where
  mf <.> mx = mkRWST $ \ r s w ->
    unRWST mf r s w >>- \(f, s', w') -> unRWST (f <$> mx) r s' w'
#endif

instance Apply (ContT r m) where
  ContT f <.> ContT v = ContT $ \k -> f $ \g -> v (k . g)

#ifdef MIN_VERSION_comonad
-- | An @'EnvT' e w@ is not 'Applicative' unless its @e@ is a 'Monoid', but it is an instance of 'Apply'
instance (Semigroup e, Apply w) => Apply (EnvT e w) where
  EnvT ef wf <.> EnvT ea wa = EnvT (ef <> ea) (wf <.> wa)

-- | A @'StoreT' s w@ is not 'Applicative' unless its @s@ is a 'Monoid', but it is an instance of 'Apply'
instance (Apply w, Semigroup s) => Apply (StoreT s w) where
  StoreT ff m <.> StoreT fa n = StoreT ((<*>) <$> ff <.> fa) (m <> n)

instance Apply w => Apply (TracedT m w) where
  TracedT wf <.> TracedT wa = TracedT (ap <$> wf <.> wa)
#endif

-- | Wrap an 'Applicative' to be used as a member of 'Apply'
newtype WrappedApplicative f a = WrapApplicative { unwrapApplicative :: f a }

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrapApplicative a) = WrapApplicative (f <$> a)

instance Applicative f => Apply (WrappedApplicative f) where
  WrapApplicative f <.> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <.  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  .> WrapApplicative b = WrapApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrapApplicative . pure
  WrapApplicative f <*> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <*  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  *> WrapApplicative b = WrapApplicative (a  *> b)

instance Alternative f => Alternative (WrappedApplicative f) where
  empty = WrapApplicative empty
  WrapApplicative a <|> WrapApplicative b = WrapApplicative (a <|> b)

-- | Transform an Apply into an Applicative by adding a unit.
newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either (f a) a }

-- | Apply a non-empty container of functions to a possibly-empty-with-unit container of values.
(<.*>) :: (Apply f) => f (a -> b) -> MaybeApply f a -> f b
ff <.*> MaybeApply (Left fa) = ff <.> fa
ff <.*> MaybeApply (Right a) = ($ a) <$> ff
infixl 4 <.*>

-- | Apply a possibly-empty-with-unit container of functions to a non-empty container of values.
(<*.>) :: (Apply f) => MaybeApply f (a -> b) -> f a -> f b
MaybeApply (Left ff) <*.> fa = ff <.> fa
MaybeApply (Right f) <*.> fa = f <$> fa
infixl 4 <*.>

-- | Traverse a 'Traversable' using 'Apply', getting the results back in a 'MaybeApply'.
traverse1Maybe :: (Traversable t, Apply f) => (a -> f b) -> t a -> MaybeApply f (t b)
traverse1Maybe f = traverse (MaybeApply . Left . f)

instance Functor f => Functor (MaybeApply f) where
  fmap f (MaybeApply (Right a)) = MaybeApply (Right (f     a ))
  fmap f (MaybeApply (Left fa)) = MaybeApply (Left  (f <$> fa))

instance Apply f => Apply (MaybeApply f) where
  MaybeApply (Right f) <.> MaybeApply (Right a) = MaybeApply (Right (f         a ))
  MaybeApply (Right f) <.> MaybeApply (Left fa) = MaybeApply (Left  (f     <$> fa))
  MaybeApply (Left ff) <.> MaybeApply (Right a) = MaybeApply (Left  (($ a) <$> ff))
  MaybeApply (Left ff) <.> MaybeApply (Left fa) = MaybeApply (Left  (ff    <.> fa))

  MaybeApply a         <. MaybeApply (Right _) = MaybeApply a
  MaybeApply (Right a) <. MaybeApply (Left fb) = MaybeApply (Left (a  <$ fb))
  MaybeApply (Left fa) <. MaybeApply (Left fb) = MaybeApply (Left (fa <. fb))

  MaybeApply (Right _) .> MaybeApply b = MaybeApply b
  MaybeApply (Left fa) .> MaybeApply (Right b) = MaybeApply (Left (fa $> b ))
  MaybeApply (Left fa) .> MaybeApply (Left fb) = MaybeApply (Left (fa .> fb))

instance Apply f => Applicative (MaybeApply f) where
  pure a = MaybeApply (Right a)
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance Extend f => Extend (MaybeApply f) where
  duplicated w@(MaybeApply Right{}) = MaybeApply (Right w)
  duplicated (MaybeApply (Left fa)) = MaybeApply (Left (extended (MaybeApply . Left) fa))

#ifdef MIN_VERSION_comonad
instance Comonad f => Comonad (MaybeApply f) where
  duplicate w@(MaybeApply Right{}) = MaybeApply (Right w)
  duplicate (MaybeApply (Left fa)) = MaybeApply (Left (extend (MaybeApply . Left) fa))
  extract (MaybeApply (Left fa)) = extract fa
  extract (MaybeApply (Right a)) = a

instance Apply (Cokleisli w a) where
  Cokleisli f <.> Cokleisli a = Cokleisli (\w -> (f w) (a w))
#endif

instance Apply Down where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

instance Apply Monoid.Sum where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Product where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Dual where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.First where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Last where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
deriving instance Apply f => Apply (Monoid.Alt f)
-- in GHC 8.6 we'll have to deal with Apply f => Apply (Ap f) the same way
instance Apply Semigroup.First where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Last where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Min where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Max where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

instance (Apply f, Apply g) => Apply (f :*: g) where
  (a :*: b) <.> (c :*: d) = (a <.> c) :*: (b <.> d)

deriving instance Apply f => Apply (M1 i t f)
deriving instance Apply f => Apply (Rec1 f)

instance (Apply f, Apply g) => Apply (f :.: g) where
  Comp1 m <.> Comp1 n = Comp1 $ (<.>) <$> m <.> n

instance Apply U1 where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

-- | A @'K1' i c@ is not 'Applicative' unless its @c@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup c => Apply (K1 i c) where
  K1 a <.> K1 b = K1 (a <> b)
  K1 a <.  K1 b = K1 (a <> b)
  K1 a  .> K1 b = K1 (a <> b)
instance Apply Par1 where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

-- | A 'V1' is not 'Applicative', but it is an instance of 'Apply'
instance Apply Generics.V1 where
  e <.> _ = case e of {}

-- | A 'Monad' sans 'return'.
--
-- Minimal definition: Either 'join' or '>>-'
--
-- If defining both, then the following laws (the default definitions) must hold:
--
-- > join = (>>- id)
-- > m >>- f = join (fmap f m)
--
-- Laws:
--
-- > induced definition of <.>: f <.> x = f >>- (<$> x)
--
-- Finally, there are two associativity conditions:
--
-- > associativity of (>>-):    (m >>- f) >>- g == m >>- (\x -> f x >>- g)
-- > associativity of join:     join . join = join . fmap join
--
-- These can both be seen as special cases of the constraint that
--
-- > associativity of (->-): (f ->- g) ->- h = f ->- (g ->- h)
--

class Apply m => Bind m where
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = join (fmap f m)

  join :: m (m a) -> m a
  join = (>>- id)

  {-# MINIMAL (>>-) | join #-}

returning :: Functor f => f a -> (a -> b) -> f b
returning = flip fmap

apDefault :: Bind f => f (a -> b) -> f a -> f b
apDefault f x = f >>- \f' -> f' <$> x

-- | A @'(,)' m@ is not a 'Monad' unless its @m@ is a 'Monoid', but it is an instance of 'Bind'
instance Semigroup m => Bind ((,) m) where
  ~(m, a) >>- f = let (n, b) = f a in (m <> n, b)

#ifdef MIN_VERSION_tagged
instance Bind (Tagged a) where
  Tagged a >>- f = f a
  join (Tagged a) = a
#endif

instance Bind Proxy where
  _ >>- _ = Proxy
  join _ = Proxy

instance Bind (Either a) where
  Left a  >>- _ = Left a
  Right a >>- f = f a

instance (Bind f, Bind g) => Bind (Functor.Product f g) where
  Functor.Pair m n >>- f = Functor.Pair (m >>- fstP . f) (n >>- sndP . f) where
    fstP (Functor.Pair a _) = a
    sndP (Functor.Pair _ b) = b

instance Bind ((->)m) where
  f >>- g = \e -> g (f e) e

instance Bind [] where
  (>>-) = (>>=)

instance Bind NonEmpty where
  (>>-) = (>>=)

instance Bind IO where
  (>>-) = (>>=)

instance Bind Maybe where
  (>>-) = (>>=)

#if !(MIN_VERSION_base(4,16,0))
instance Bind Option where
  (>>-) = (>>=)
#endif

instance Bind Identity where
  (>>-) = (>>=)

instance Bind Q where
  (>>-) = (>>=)

instance Bind m => Bind (IdentityT m) where
  IdentityT m >>- f = IdentityT (m >>- runIdentityT . f)

instance Monad m => Bind (WrappedMonad m) where
  WrapMonad m >>- f = WrapMonad $ m >>= unwrapMonad . f

instance (Functor m, Monad m) => Bind (MaybeT m) where
  (>>-) = (>>=) -- distributive law requires Monad to inject @Nothing@

#if !(MIN_VERSION_transformers(0,6,0))
instance (Apply m, Monad m) => Bind (ListT m) where
  (>>-) = (>>=) -- distributive law requires Monad to inject @[]@

instance (Functor m, Monad m) => Bind (ErrorT e m) where
  m >>- k = ErrorT $ do
    a <- runErrorT m
    case a of
      Left l -> return (Left l)
      Right r -> runErrorT (k r)
#endif

instance (Functor m, Monad m) => Bind (ExceptT e m) where
  m >>- k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left l -> return (Left l)
      Right r -> runExceptT (k r)

instance Bind m => Bind (ReaderT e m) where
  ReaderT m >>- f = ReaderT $ \e -> m e >>- \x -> runReaderT (f x) e

-- | A @'Lazy.WriterT' w m@ is not a 'Monad' unless its @w@ is a 'Monoid', but it is an instance of 'Bind'
instance (Bind m, Semigroup w) => Bind (Lazy.WriterT w m) where
  m >>- k = Lazy.WriterT $
    Lazy.runWriterT m >>- \ ~(a, w) ->
    Lazy.runWriterT (k a) `returning` \ ~(b, w') ->
      (b, w <> w')

-- | A @'Strict.WriterT' w m@ is not a 'Monad' unless its @w@ is a 'Monoid', but it is an instance of 'Bind'
instance (Bind m, Semigroup w) => Bind (Strict.WriterT w m) where
  m >>- k = Strict.WriterT $
    Strict.runWriterT m >>- \ (a, w) ->
    Strict.runWriterT (k a) `returning` \ (b, w') ->
      (b, w <> w')

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Bind m) => Bind (CPS.WriterT w m) where
  m >>- k = mkWriterT $ \ w ->
    unWriterT m w >>- \(a, w') -> unWriterT (k a) w'
#endif

instance Bind m => Bind (Lazy.StateT s m) where
  m >>- k = Lazy.StateT $ \s ->
    Lazy.runStateT m s >>- \ ~(a, s') ->
    Lazy.runStateT (k a) s'

instance Bind m => Bind (Strict.StateT s m) where
  m >>- k = Strict.StateT $ \s ->
    Strict.runStateT m s >>- \ ~(a, s') ->
    Strict.runStateT (k a) s'

-- | An @'Lazy.RWST' r w s m@ is not a 'Monad' unless its @w@ is a 'Monoid', but it is an instance of 'Bind'
instance (Bind m, Semigroup w) => Bind (Lazy.RWST r w s m) where
  m >>- k = Lazy.RWST $ \r s ->
    Lazy.runRWST m r s >>- \ ~(a, s', w) ->
    Lazy.runRWST (k a) r s' `returning` \ ~(b, s'', w') ->
      (b, s'', w <> w')

-- | An @'Strict.RWST' r w s m@ is not a 'Monad' unless its @w@ is a 'Monoid', but it is an instance of 'Bind'
instance (Bind m, Semigroup w) => Bind (Strict.RWST r w s m) where
  m >>- k = Strict.RWST $ \r s ->
    Strict.runRWST m r s >>- \ (a, s', w) ->
    Strict.runRWST (k a) r s' `returning` \ (b, s'', w') ->
      (b, s'', w <> w')

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance (Bind m) => Bind (CPS.RWST r w s m) where
  m >>- k = mkRWST $ \ r s w ->
    unRWST m r s w >>- \(a, s', w') -> unRWST (k a) r s' w'
#endif

instance Bind (ContT r m) where
  m >>- k = ContT $ \c -> runContT m $ \a -> runContT (k a) c

instance Bind Complex where
  (a :+ b) >>- f = a' :+ b' where
    a' :+ _  = f a
    _  :+ b' = f b
  {-# INLINE (>>-) #-}

#ifdef MIN_VERSION_containers
-- | A 'Map k' is not a 'Monad', but it is an instance of 'Bind'
instance Ord k => Bind (Map k) where
  m >>- f = Map.mapMaybeWithKey (\k -> Map.lookup k . f) m

-- | An 'IntMap' is not a 'Monad', but it is an instance of 'Bind'
instance Bind IntMap where
  m >>- f = IntMap.mapMaybeWithKey (\k -> IntMap.lookup k . f) m

instance Bind Seq where
  (>>-) = (>>=)

instance Bind Tree where
  (>>-) = (>>=)
#endif

#ifdef MIN_VERSION_unordered_containers
-- | A 'HashMap k' is not a 'Monad', but it is an instance of 'Bind'
instance (Hashable k, Eq k) => Bind (HashMap k) where
  -- this is needlessly painful
  m >>- f = HashMap.fromList $ do
    (k, a) <- HashMap.toList m
    case HashMap.lookup k (f a) of
      Just b -> [(k,b)]
      Nothing -> []
#endif

instance Bind Down where Down a >>- f = f a

instance Bind Monoid.Sum where (>>-) = (>>=)
instance Bind Monoid.Product where (>>-) = (>>=)
instance Bind Monoid.Dual where (>>-) = (>>=)
instance Bind Monoid.First where (>>-) = (>>=)
instance Bind Monoid.Last where (>>-) = (>>=)
instance Bind f => Bind (Monoid.Alt f) where
  Monoid.Alt m >>- k = Monoid.Alt (m >>- Monoid.getAlt . k)
-- in GHC 8.6 we'll have to deal with Bind f => Bind (Ap f) the same way
instance Bind Semigroup.First where (>>-) = (>>=)
instance Bind Semigroup.Last where (>>-) = (>>=)
instance Bind Semigroup.Min where (>>-) = (>>=)
instance Bind Semigroup.Max where (>>-) = (>>=)
-- | A 'V1' is not a 'Monad', but it is an instance of 'Bind'
instance Bind Generics.V1 where
  m >>- _ = case m of {}

-- | @since 5.3.8
instance Bind Generics.U1 where (>>-)=(>>=)

-- | @since 5.3.8
instance Bind f => Bind (Generics.M1 i c f) where
  M1 m >>- f = M1 $ m >>- \a -> case f a of
    M1 m' -> m'

-- | @since 5.3.8
instance Bind m => Bind (Generics.Rec1 m) where
  Rec1 m >>- f = Rec1 $ m >>- \a -> case f a of
    Rec1 m' -> m'

-- | @since 5.3.8
instance Bind Generics.Par1 where
  Par1 m >>- f = f m

-- | @since 5.3.8
instance (Bind f, Bind g) => Bind (f :*: g) where
  m :*: n >>- f = (m >>- fstP . f) :*: (n >>- sndP . f) where
    fstP (a :*: _) = a
    sndP (_ :*: b) = b

infixl 4 <<.>>, <<., .>>

class Bifunctor p => Biapply p where
  (<<.>>) :: p (a -> b) (c -> d) -> p a c -> p b d

  -- |
  -- @
  -- a '.>' b ≡ 'const' 'id' '<$>' a '<.>' b
  -- @
  (.>>) :: p a b -> p c d -> p c d
  a .>> b = bimap (const id) (const id) <<$>> a <<.>> b
  {-# INLINE (.>>) #-}

  -- |
  -- @
  -- a '<.' b ≡ 'const' '<$>' a '<.>' b
  -- @
  (<<.) :: p a b -> p c d -> p a b
  a <<. b = bimap const const <<$>> a <<.>> b
  {-# INLINE (<<.) #-}

instance Biapply (,) where
  (f, g) <<.>> (a, b) = (f a, g b)
  {-# INLINE (<<.>>) #-}

instance Biapply Arg where
  Arg f g <<.>> Arg a b = Arg (f a) (g b)
  {-# INLINE (<<.>>) #-}

instance Semigroup x => Biapply ((,,) x) where
  (x, f, g) <<.>> (x', a, b) = (x <> x', f a, g b)
  {-# INLINE (<<.>>) #-}

instance (Semigroup x, Semigroup y) => Biapply ((,,,) x y) where
  (x, y, f, g) <<.>> (x', y', a, b) = (x <> x', y <> y', f a, g b)
  {-# INLINE (<<.>>) #-}

instance (Semigroup x, Semigroup y, Semigroup z) => Biapply ((,,,,) x y z) where
  (x, y, z, f, g) <<.>> (x', y', z', a, b) = (x <> x', y <> y', z <> z', f a, g b)
  {-# INLINE (<<.>>) #-}

instance Biapply Const where
  Const f <<.>> Const x = Const (f x)
  {-# INLINE (<<.>>) #-}

#ifdef MIN_VERSION_tagged
instance Biapply Tagged where
  Tagged f <<.>> Tagged x = Tagged (f x)
  {-# INLINE (<<.>>) #-}
#endif

instance (Biapply p, Apply f, Apply g) => Biapply (Biff p f g) where
  Biff fg <<.>> Biff xy = Biff (bimap (<.>) (<.>) fg <<.>> xy)
  {-# INLINE (<<.>>) #-}

instance Apply f => Biapply (Clown f) where
  Clown fg <<.>> Clown xy = Clown (fg <.> xy)
  {-# INLINE (<<.>>) #-}

instance Biapply p => Biapply (Flip p) where
  Flip fg <<.>> Flip xy = Flip (fg <<.>> xy)
  {-# INLINE (<<.>>) #-}

instance Apply g => Biapply (Joker g) where
  Joker fg <<.>> Joker xy = Joker (fg <.> xy)
  {-# INLINE (<<.>>) #-}

-- orphan mess
instance Biapply p => Apply (Join p) where
  Join f <.> Join a = Join (f <<.>> a)
  {-# INLINE (<.>) #-}
  Join a .> Join b = Join (a .>> b)
  {-# INLINE (.>) #-}
  Join a <. Join b = Join (a <<. b)
  {-# INLINE (<.) #-}

instance (Biapply p, Biapply q) => Biapply (Bifunctor.Product p q) where
  Bifunctor.Pair w x <<.>> Bifunctor.Pair y z = Bifunctor.Pair (w <<.>> y) (x <<.>> z)
  {-# INLINE (<<.>>) #-}

instance (Apply f, Biapply p) => Biapply (Tannen f p) where
  Tannen fg <<.>> Tannen xy = Tannen ((<<.>>) <$> fg <.> xy)
  {-# INLINE (<<.>>) #-}

instance Biapply p => Biapply (WrappedBifunctor p) where
  WrapBifunctor fg <<.>> WrapBifunctor xy = WrapBifunctor (fg <<.>> xy)
  {-# INLINE (<<.>>) #-}
