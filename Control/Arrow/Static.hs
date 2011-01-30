{-# LANGUAGE CPP #-}
module Control.Arrow.Static where

import Control.Monad.Instances
import Control.Monad (ap)
import Prelude hiding ((.), id)
import Data.Semigroupoid
import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Plus
import Control.Category
import Control.Arrow

#ifdef LANGUAGE_DeriveDataTypeable 
import Data.Typeable
#endif

newtype Static f a b = Static { runStatic :: f (a -> b) } 
#ifdef LANGUAGE_DeriveDataTypeable
  deriving (Typeable)
#endif

instance Functor f => Functor (Static f a) where
  fmap f = Static . fmap (f .) . runStatic

instance Apply f => Apply (Static f a) where
  Static f <.> Static g = Static (ap <$> f <.> g)

instance Alt f => Alt (Static f a) where
  Static f <!> Static g = Static (f <!> g)

instance Plus f => Plus (Static f a) where
  zero = Static zero

instance Applicative f => Applicative (Static f a) where
  pure = Static . pure . const 
  Static f <*> Static g = Static (ap <$> f <*> g)

instance Apply f => Semigroupoid (Static f) where
  Static f `o` Static g = Static ((.) <$> f <.> g)

instance Applicative f => Category (Static f) where
  id = Static (pure id)
  Static f . Static g = Static ((.) <$> f <*> g)

instance Applicative f => Arrow (Static f) where
  arr = Static . pure 
  first (Static g) = Static (first <$> g) 
  second (Static g) = Static (second <$> g) 
  Static g *** Static h = Static ((***) <$> g <*> h)
  Static g &&& Static h = Static ((&&&) <$> g <*> h)

instance Alternative f => ArrowZero (Static f) where
  zeroArrow = Static empty
  
instance Alternative f => ArrowPlus (Static f) where
  Static f <+> Static g = Static (f <|> g)

instance Applicative f => ArrowChoice (Static f) where
  left (Static g) = Static (left <$> g)
  right (Static g) = Static (right <$> g)
  Static g +++ Static h = Static ((+++) <$> g <*> h)
  Static g ||| Static h = Static ((|||) <$> g <*> h)

