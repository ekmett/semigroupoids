{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

module Semigroupoids.Internal where

#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Unsafe.Coerce (unsafeCoerce)
#endif

-- This is designed to avoid both https://hub.darcs.net/ross/transformers/issue/67
-- and also the unnecessary Monoid constraints that the CPS versions of WriterT
-- and RWST require.

#if MIN_VERSION_transformers(0,5,6)
mkWriterT :: (w -> m (a, w)) -> CPS.WriterT w m a
mkWriterT = unsafeCoerce

unWriterT :: CPS.WriterT w m a -> w -> m (a, w)
unWriterT = unsafeCoerce

mkRWST :: (r -> s -> w -> m (a, s, w)) -> CPS.RWST r w s m a
mkRWST = unsafeCoerce

unRWST :: CPS.RWST r w s m a -> r -> s -> w -> m (a, s, w)
unRWST = unsafeCoerce
#endif
