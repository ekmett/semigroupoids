{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Bind
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- NB: The definitions exported through "Data.Functor.Apply" need to be 
-- included here because otherwise the instances for the transformers package
-- have orphaned heads.
----------------------------------------------------------------------------
module Data.Functor.Bind ( 
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b 
  -- * Applyable functors
  , Apply(..)
  , (<..>)    -- :: Apply w => w a -> w (a -> b) -> w b
  , liftF2    -- :: Apply w => (a -> b -> c) -> w a -> w b -> w c
  , liftF3    -- :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  -- * Bindable functors
  , Bind(..)
  , (-<<)
  , (-<-)
  , (->-)
  , apDefault
  , returning
  ) where

-- import _everything_
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Comonad
import Control.Monad (ap)
import Control.Monad.Instances
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty
import Data.Semigroup hiding (Product)
import Data.Sequence (Seq)
import Data.Tree (Tree)
import Prelude hiding (id, (.))

infixl 1 >>-
infixr 1 -<<
infixl 4 <.>, <., .>, <..>, $>

-- | TODO: move into Data.Functor
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | A strong lax semi-monoidal endofunctor. 
-- This is equivalent to an 'Applicative' without 'pure'.
-- 
-- Laws: 
--
-- > associative composition: (.) <$> u <.> v <.> w = u <.> (v <.> w)
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b

  -- | > a  .> b = const id <$> a <.> b
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | > a <. b = const <$> a <.> b
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

instance (Apply f, Apply g) => Apply (Compose f g) where
  Compose f <.> Compose x = Compose ((<.>) <$> f <.> x) 

instance (Apply f, Apply g) => Apply (Product f g) where
  Pair f g <.> Pair x y = Pair (f <.> x) (g <.> y)

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

instance Apply Option where
  (<.>) = (<*>)
  (<. ) = (<* ) 
  ( .>) = ( *>)

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

-- | A Map is not 'Applicative', but it is an instance of 'Apply'
instance Ord k => Apply (Map k) where
  (<.>) = Map.intersectionWith id
  (<. ) = Map.intersectionWith const
  ( .>) = Map.intersectionWith (const id)

-- | An IntMap is not 'Applicative', but it is an instance of 'Apply'
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

-- MaybeT is _not_ the same as Compose f Maybe
instance (Bind m, Monad m) => Apply (MaybeT m) where
  (<.>) = apDefault

-- ErrorT e is _not_ the same as Compose f (Either e)
instance (Bind m, Monad m) => Apply (ErrorT e m) where
  (<.>) = apDefault

instance Apply m => Apply (ReaderT e m) where
  ReaderT f <.> ReaderT a = ReaderT $ \e -> f e <.> a e 

instance Apply m => Apply (ListT m) where
  ListT f <.> ListT a = ListT $ (<.>) <$> f <.> a

-- unfortunately, WriterT has its wrapped product in the wrong order to just use (<.>) instead of flap
instance (Apply m, Semigroup w) => Apply (Strict.WriterT w m) where
  Strict.WriterT f <.> Strict.WriterT a = Strict.WriterT $ flap <$> f <.> a where
    flap (x,m) (y,n) = (x y, m <> n)

instance (Apply m, Semigroup w) => Apply (Lazy.WriterT w m) where
  Lazy.WriterT f <.> Lazy.WriterT a = Lazy.WriterT $ flap <$> f <.> a where
    flap ~(x,m) ~(y,n) = (x y, m <> n)
  
instance Bind m => Apply (Strict.StateT s m) where
  (<.>) = apDefault

instance Bind m => Apply (Lazy.StateT s m) where
  (<.>) = apDefault

instance (Bind m, Semigroup w) => Apply (Strict.RWST r w s m) where
  (<.>) = apDefault

instance (Bind m, Semigroup w) => Apply (Lazy.RWST r w s m) where
  (<.>) = apDefault

instance Apply (ContT r m) where
  ContT f <.> ContT v = ContT $ \k -> f $ \g -> v (k . g)

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

-- | Transform a Apply into an Applicative by adding a unit.
newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either (f a) a }

instance Functor f => Functor (MaybeApply f) where
  fmap f (MaybeApply (Right a)) = MaybeApply (Right (f     a ))
  fmap f (MaybeApply (Left fa)) = MaybeApply (Left  (f <$> fa))

instance Apply f => Apply (MaybeApply f) where
  MaybeApply (Right f) <.> MaybeApply (Right a) = MaybeApply (Right (f        a ))
  MaybeApply (Right f) <.> MaybeApply (Left fa) = MaybeApply (Left  (f    <$> fa))
  MaybeApply (Left ff) <.> MaybeApply (Right a) = MaybeApply (Left  (($a) <$> ff))
  MaybeApply (Left ff) <.> MaybeApply (Left fa) = MaybeApply (Left  (ff   <.> fa))

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

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: Apply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}

-- | Lift a binary function into a comonad with zipping
liftF2 :: Apply w => (a -> b -> c) -> w a -> w b -> w c
liftF2 f a b = f <$> a <.> b
{-# INLINE liftF2 #-}

-- | Lift a ternary function into a comonad with zipping
liftF3 :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

instance Extend f => Extend (MaybeApply f) where
  duplicate w@(MaybeApply Right{}) = MaybeApply (Right w)
  duplicate (MaybeApply (Left fa)) = MaybeApply (Left (extend (MaybeApply . Left) fa))

instance Comonad f => Comonad (MaybeApply f) where
  extract (MaybeApply (Left fa)) = extract fa
  extract (MaybeApply (Right a)) = a

instance Apply (Cokleisli w a) where
  Cokleisli f <.> Cokleisli a = Cokleisli (\w -> (f w) (a w))

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

returning :: Functor f => f a -> (a -> b) -> f b
returning = flip fmap

(-<<) :: Bind m => (a -> m b) -> m a -> m b
(-<<) = flip (>>-)

(->-) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f ->- g = \a -> f a >>- g

(-<-) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g -<- f = \a -> f a >>- g

apDefault :: Bind f => f (a -> b) -> f a -> f b
apDefault f x = f >>- \f' -> f' <$> x

instance Semigroup m => Bind ((,)m) where
  ~(m, a) >>- f = let (n, b) = f a in (m <> n, b)

instance Bind (Either a) where
  Left a  >>- _ = Left a
  Right a >>- f = f a 

instance (Bind f, Bind g) => Bind (Product f g) where
  Pair m n >>- f = Pair (m >>- fstP . f) (n >>- sndP . f) where
    fstP (Pair a _) = a
    sndP (Pair _ b) = b

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

instance Bind Option where
  (>>-) = (>>=)

instance Bind Identity where
  (>>-) = (>>=)

instance Bind m => Bind (IdentityT m) where
  IdentityT m >>- f = IdentityT (m >>- runIdentityT . f)

instance Monad m => Bind (WrappedMonad m) where
  WrapMonad m >>- f = WrapMonad $ m >>= unwrapMonad . f 

instance (Bind m, Monad m) => Bind (MaybeT m) where
  (>>-) = (>>=) -- distributive law requires Monad to inject @Nothing@

instance (Bind m, Monad m) => Bind (ListT m) where
  (>>-) = (>>=) -- distributive law requires Monad to inject @[]@

instance (Bind m, Monad m) => Bind (ErrorT e m) where
  m >>- k = ErrorT $ do
    a <- runErrorT m 
    case a of
      Left l -> return (Left l)
      Right r -> runErrorT (k r)

instance Bind m => Bind (ReaderT e m) where
  ReaderT m >>- f = ReaderT $ \e -> m e >>- \x -> runReaderT (f x) e

instance (Bind m, Semigroup w) => Bind (Lazy.WriterT w m) where
  m >>- k = Lazy.WriterT $
    Lazy.runWriterT m >>- \ ~(a, w) -> 
    Lazy.runWriterT (k a) `returning` \ ~(b, w') -> 
      (b, w <> w')

instance (Bind m, Semigroup w) => Bind (Strict.WriterT w m) where
  m >>- k = Strict.WriterT $
    Strict.runWriterT m >>- \ (a, w) -> 
    Strict.runWriterT (k a) `returning` \ (b, w') -> 
      (b, w <> w')

instance Bind m => Bind (Lazy.StateT s m) where
  m >>- k = Lazy.StateT $ \s -> 
    Lazy.runStateT m s >>- \ ~(a, s') ->
    Lazy.runStateT (k a) s'

instance Bind m => Bind (Strict.StateT s m) where
  m >>- k = Strict.StateT $ \s -> 
    Strict.runStateT m s >>- \ ~(a, s') ->
    Strict.runStateT (k a) s'
    
instance (Bind m, Semigroup w) => Bind (Lazy.RWST r w s m) where
  m >>- k = Lazy.RWST $ \r s -> 
    Lazy.runRWST m r s >>- \ ~(a, s', w) ->
    Lazy.runRWST (k a) r s' `returning` \ ~(b, s'', w') -> 
      (b, s'', w <> w')

instance (Bind m, Semigroup w) => Bind (Strict.RWST r w s m) where
  m >>- k = Strict.RWST $ \r s -> 
    Strict.runRWST m r s >>- \ (a, s', w) ->
    Strict.runRWST (k a) r s' `returning` \ (b, s'', w') -> 
      (b, s'', w <> w')

instance Bind (ContT r m) where
  m >>- k = ContT $ \c -> runContT m $ \a -> runContT (k a) c

{-
instance ArrowApply a => Bind (WrappedArrow a b) where
  (>>-) = (>>=)
-}

-- | A 'Map' is not a 'Monad', but it is an instance of 'Bind'
instance Ord k => Bind (Map k) where
  m >>- f = Map.mapMaybeWithKey (\k -> Map.lookup k . f) m

-- | An 'IntMap' is a 'Applicative', but it is an instance of 'Bind'
instance Bind IntMap where
  m >>- f = IntMap.mapMaybeWithKey (\k -> IntMap.lookup k . f) m

instance Bind Seq where
  (>>-) = (>>=)

instance Bind Tree where
  (>>-) = (>>=)

instance (Comonad w, Apply w) => ArrowLoop (Cokleisli w) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where
    f' wa wb = f ((,) <$> wa <.> (snd <$> wb))
