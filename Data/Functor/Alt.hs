-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Alt
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Alt
  ( FunctorAlt(..)
  , module Data.Functor.Apply
  ) where

import Prelude hiding (id, (.))
import Control.Applicative hiding (some, many)
import Control.Arrow
-- import Control.Exception
import Control.Monad
import Data.Functor.Apply
import Data.Semigroup
import Data.Monoid

-- instances
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Sequence (Seq)

infixl 3 <!> 

class FunctorApply f => FunctorAlt f where
  (<!>) :: f a -> f a -> f a

instance FunctorAlt (Either a) where
  Left _ <!> b = b
  a      <!> _ = a

instance FunctorAlt IO where
  m <!> n = m `catch` \_ -> n

instance FunctorAlt [] where
  (<!>) = (++)

instance FunctorAlt Maybe where
  Nothing <!> b = b
  a       <!> _ = a

instance FunctorAlt Option where
  (<!>) = (<|>)

instance MonadPlus m => FunctorAlt (WrappedMonad m) where
  (<!>) = (<|>)

instance ArrowPlus a => FunctorAlt (WrappedArrow a b) where
  (<!>) = (<|>) 

instance Ord k => FunctorAlt (Map k) where
  (<!>) = Map.union

instance FunctorAlt IntMap where
  (<!>) = IntMap.union

instance FunctorAlt Seq where
  (<!>) = mappend

instance Alternative f => FunctorAlt (WrappedApplicative f) where
  WrapApplicative a <!> WrapApplicative b = WrapApplicative (a <|> b)
