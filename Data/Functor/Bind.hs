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
module Data.Functor.Bind ( 
    Bind(..)
  , (-<<)
  , (-<-)
  , (->-)
  , apDefault
  , module Data.Functor.Apply
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Applicative
import Data.Functor.Apply
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

infixl 1 >>-
infixr 1 -<<

class Apply m => Bind m where
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = join (fmap f m)

  join :: m (m a) -> m a
  join = (>>- id)

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

instance Bind ((->)m) where
  f >>- g = \e -> g (f e) e 

instance Bind [] where
  (>>-) = (>>=)

instance Bind IO where
  (>>-) = (>>=)

instance Bind Maybe where
  (>>-) = (>>=)

instance Bind Option where
  (>>-) = (>>=)

instance Bind Identity where
  (>>-) = (>>=)

instance Bind w => Bind (IdentityT w) where
  IdentityT m >>- f = IdentityT (m >>- runIdentityT . f)

instance Monad m => Bind (WrappedMonad m) where
  WrapMonad m >>- f = WrapMonad $ m >>= unwrapMonad . f 

{-
instance ArrowApply a => Bind (WrappedArrow a b) where
  (>>-) = (>>=)
-}

-- | A Map is not 'Applicative', but it is an instance of 'Apply'
instance Ord k => Bind (Map k) where
  m >>- f = Map.mapMaybeWithKey (\k -> Map.lookup k . f) m

-- | An IntMap is not Applicative, but it is an instance of 'Apply'
instance Bind IntMap where
  m >>- f = IntMap.mapMaybeWithKey (\k -> IntMap.lookup k . f) m

instance Bind Seq where
  (>>-) = (>>=)

instance Bind Tree where
  (>>-) = (>>=)
