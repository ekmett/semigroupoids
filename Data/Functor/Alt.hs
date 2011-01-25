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
  ( Alt(..)
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

class Apply f => Alt f where
  (<!>) :: f a -> f a -> f a

instance Alt (Either a) where
  Left _ <!> b = b
  a      <!> _ = a

instance Alt IO where
  m <!> n = m `catch` \_ -> n

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

instance Ord k => Alt (Map k) where
  (<!>) = Map.union

instance Alt IntMap where
  (<!>) = IntMap.union

instance Alt Seq where
  (<!>) = mappend

instance Alternative f => Alt (WrappedApplicative f) where
  WrapApplicative a <!> WrapApplicative b = WrapApplicative (a <|> b)
