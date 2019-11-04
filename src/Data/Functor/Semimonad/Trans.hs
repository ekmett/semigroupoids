{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Semimonad.Trans
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Semimonad.Trans (
  SemimonadTrans(..)
  ) where

-- import _everything_
import Control.Category
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
-- import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.List
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Semimonad
import Data.Orphans ()
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup hiding (Product)
#endif
import Prelude hiding (id, (.))

-- | A subset of monad transformers can transform any 'Semimonad' as well.
class MonadTrans t => SemimonadTrans t where
  liftB :: Semimonad b => b a -> t b a

instance SemimonadTrans IdentityT where
  liftB = IdentityT

instance SemimonadTrans (ReaderT e) where
  liftB = ReaderT . const

instance Monoid w => SemimonadTrans (Lazy.WriterT w) where
  liftB = Lazy.WriterT . fmap (\a -> (a, mempty))

instance Monoid w => SemimonadTrans (Strict.WriterT w) where
  liftB = Strict.WriterT . fmap (\a -> (a, mempty))

instance SemimonadTrans (Lazy.StateT s) where
  liftB m = Lazy.StateT $ \s -> fmap (\a -> (a, s)) m

instance SemimonadTrans (Strict.StateT s) where
  liftB m = Strict.StateT $ \s -> fmap (\a -> (a, s)) m

instance Monoid w => SemimonadTrans (Lazy.RWST r w s) where
  liftB m = Lazy.RWST $ \ _r s -> fmap (\a -> (a, s, mempty)) m

instance Monoid w => SemimonadTrans (Strict.RWST r w s) where
  liftB m = Strict.RWST $ \ _r s -> fmap (\a -> (a, s, mempty)) m

instance SemimonadTrans (ContT r) where
  liftB m = ContT (m >>-)
