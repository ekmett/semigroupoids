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

  -- * Apply - a strong lax semimonoidal endofunctor

  , Apply(..)
  , (<..>)    -- :: Apply w => w a -> w (a -> b) -> w b
  , liftF2    -- :: Apply w => (a -> b -> c) -> w a -> w b -> w c
  , liftF3    -- :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

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
import Data.Functor.Extend
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

class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b

  -- | a .> b = const id <$> a <.> b
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | a <. b = const <$> a <.> b
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

instance Semigroup m => Apply ((,)m) where
  (m, f) <.> (n, a) = (m <> n, f a)
  (m, a) <.  (n, _) = (m <> n, a) 
  (m, _)  .> (n, b) = (m <> n, b)

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

-- | An IntMap is not Applicative, but it is an instance of 'Apply'
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
