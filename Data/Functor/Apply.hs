-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Apply
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Apply ( 
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b 

  -- * FunctorApply - a strong lax semimonoidal endofunctor

  , FunctorApply(..)
  , (<..>)    -- :: FunctorApply w => w a -> w (a -> b) -> w b
  , liftF2    -- :: FunctorApply w => (a -> b -> c) -> w a -> w b -> w c
  , liftF3    -- :: FunctorApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

  -- * Wrappers
  , WrappedApplicative(..)
  , MaybeApply(..)
  ) where

import Prelude hiding (id, (.))
import Control.Applicative hiding (some, many)
import qualified Control.Applicative as A
import Control.Arrow
import Control.Comonad
import Control.Category
import Control.Monad (ap)
import Control.Monad.Instances
import Data.Functor
import Data.Semigroup

-- instances
import Data.Functor.Identity
import Control.Monad.Trans.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import Data.Tree (Tree)

infixl 4 <.>, <., .>, <..>, $>

-- | TODO: move into Data.Functor
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | A strong lax semi-monoidal endofunctor

class Functor f => FunctorApply f where
  (<.>) :: f (a -> b) -> f a -> f b

  -- | a .> b = const id <$> a <.> b
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | a <. b = const <$> a <.> b
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

instance Semigroup m => FunctorApply ((,)m) where
  (m, f) <.> (n, a) = (m <> n, f a)
  (m, a) <.  (n, _) = (m <> n, a) 
  (m, _)  .> (n, b) = (m <> n, b)

instance FunctorApply (Either a) where
  Left a  <.> _       = Left a
  Right _ <.> Left a  = Left a
  Right f <.> Right b = Right (f b)

  Left a  <.  _       = Left a
  Right _ <.  Left a  = Left a
  Right a <.  Right _ = Right a

  Left a   .> _       = Left a
  Right _  .> Left a  = Left a
  Right _  .> Right b = Right b

instance Semigroup m => FunctorApply (Const m) where
  Const m <.> Const n = Const (m <> n)
  Const m <.  Const n = Const (m <> n)
  Const m  .> Const n = Const (m <> n)

instance FunctorApply ((->)m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply ZipList where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply [] where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply IO where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply Maybe where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply Option where
  (<.>) = (<*>)
  (<. ) = (<* ) 
  ( .>) = ( *>)

instance FunctorApply Identity where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply w => FunctorApply (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

instance Monad m => FunctorApply (WrappedMonad m) where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

instance Arrow a => FunctorApply (WrappedArrow a b) where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

-- | A Map is not 'Applicative', but it is an instance of 'FunctorApply'
instance Ord k => FunctorApply (Map k) where
  (<.>) = Map.intersectionWith id
  (<. ) = Map.intersectionWith const
  ( .>) = Map.intersectionWith (const id)

-- | An IntMap is not Applicative, but it is an instance of 'FunctorApply'
instance FunctorApply IntMap where
  (<.>) = IntMap.intersectionWith id
  (<. ) = IntMap.intersectionWith const
  ( .>) = IntMap.intersectionWith (const id)

instance FunctorApply Seq where
  (<.>) = ap

instance FunctorApply Tree where
  (<.>) = (<*>) 
  (<. ) = (<* ) 
  ( .>) = ( *>) 

-- | Wrap an 'Applicative' to be used as a member of 'FunctorApply'
newtype WrappedApplicative f a = WrapApplicative { unwrapApplicative :: f a } 

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrapApplicative a) = WrapApplicative (f <$> a)

instance Applicative f => FunctorApply (WrappedApplicative f) where
  WrapApplicative f <.> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <.  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  .> WrapApplicative b = WrapApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrapApplicative . pure
  WrapApplicative f <*> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <*  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  *> WrapApplicative b = WrapApplicative (a  *> b)

-- | Transform a FunctorApply into an Applicative by adding a unit.
newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either (f a) a }

instance Functor f => Functor (MaybeApply f) where
  fmap f (MaybeApply (Right a)) = MaybeApply (Right (f     a ))
  fmap f (MaybeApply (Left fa)) = MaybeApply (Left  (f <$> fa))

instance FunctorApply f => FunctorApply (MaybeApply f) where
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
  
instance FunctorApply f => Applicative (MaybeApply f) where
  pure a = MaybeApply (Right a)
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: FunctorApply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}

-- | Lift a binary function into a comonad with zipping
liftF2 :: FunctorApply w => (a -> b -> c) -> w a -> w b -> w c
liftF2 f a b = f <$> a <.> b
{-# INLINE liftF2 #-}

-- | Lift a ternary function into a comonad with zipping
liftF3 :: FunctorApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

instance Comonad f => Comonad (MaybeApply f) where
  extract (MaybeApply (Right a)) = a
  extract (MaybeApply (Left fa)) = extract fa
  duplicate w@(MaybeApply Right{}) = MaybeApply (Right w)
  duplicate (MaybeApply (Left fa)) = MaybeApply (Left (extend (MaybeApply . Left) fa))

instance FunctorApply (Cokleisli w a) where
  Cokleisli f <.> Cokleisli a = Cokleisli (\w -> (f w) (a w))
