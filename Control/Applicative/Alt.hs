-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Alt
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Applicative.Alt ( 
    ApplicativeAlt(..)
  , module Data.Functor.Alt
  ) where

import Control.Applicative hiding (some, many)
import Control.Arrow
import Control.Monad
import Data.Functor.Alt
import Data.Semigroup
-- import Data.Sequence (Seq)

class (FunctorAlt f, Applicative f) => ApplicativeAlt f where
  some :: f a -> f [a]
  some v = some_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <.> many_v

  many :: f a -> f [a]
  many v = many_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <.> many_v

-- base is missing Alternative (Either a)
-- instance ApplicativeAlt (Either a) 

instance ApplicativeAlt []
instance ApplicativeAlt IO where
instance ApplicativeAlt Maybe
instance ApplicativeAlt Option
instance MonadPlus m => ApplicativeAlt (WrappedMonad m)
instance ArrowPlus a => ApplicativeAlt (WrappedArrow a b)
-- instance ApplicativeAlt Seq
instance Alternative f => ApplicativeAlt (WrappedApplicative f)
