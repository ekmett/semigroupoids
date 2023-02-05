{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Bind.Trans
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Bind.Trans (
  BindTrans(..)
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
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
#endif
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Bind
import Data.Orphans ()
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup hiding (Product)
#endif
import Prelude hiding (id, (.))

-- | A subset of monad transformers can transform any 'Bind' as well.
class MonadTrans t => BindTrans t where
  liftB :: Bind b => b a -> t b a

instance BindTrans IdentityT where
  liftB = IdentityT

instance BindTrans (ReaderT e) where
  liftB = ReaderT . const

instance Monoid w => BindTrans (Lazy.WriterT w) where
  liftB = Lazy.WriterT . fmap (\a -> (a, mempty))

instance Monoid w => BindTrans (Strict.WriterT w) where
  liftB = Strict.WriterT . fmap (\a -> (a, mempty))

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance Monoid w => BindTrans (CPS.WriterT w) where
  liftB = CPS.writerT . fmap (\a -> (a, mempty))
#endif

instance BindTrans (Lazy.StateT s) where
  liftB m = Lazy.StateT $ \s -> fmap (\a -> (a, s)) m

instance BindTrans (Strict.StateT s) where
  liftB m = Strict.StateT $ \s -> fmap (\a -> (a, s)) m

instance Monoid w => BindTrans (Lazy.RWST r w s) where
  liftB m = Lazy.RWST $ \ _r s -> fmap (\a -> (a, s, mempty)) m

instance Monoid w => BindTrans (Strict.RWST r w s) where
  liftB m = Strict.RWST $ \ _r s -> fmap (\a -> (a, s, mempty)) m

#if MIN_VERSION_transformers(0,5,6)
-- | @since 5.3.6
instance Monoid w => BindTrans (CPS.RWST r w s) where
  liftB m = CPS.rwsT $ \ _r s -> fmap (\a -> (a, s, mempty)) m
#endif

instance BindTrans (ContT r) where
  liftB m = ContT (m >>-)
