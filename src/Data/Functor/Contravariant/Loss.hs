{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Contravariant.Loss (
    Loss(..)
  , lossed
  ) where

import Control.Applicative.Backwards
import Control.Arrow
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import Data.Either
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Decide
import Data.Functor.Contravariant.Divise
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Void

#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt(..))
#else
import Data.Monoid (Monoid(..))
#endif

#if MIN_VERSION_base(4,7,0) || defined(MIN_VERSION_tagged)
import Data.Proxy
#endif

#ifdef MIN_VERSION_StateVar
import Data.StateVar
#endif

#if __GLASGOW_HASKELL__ >= 702
#define GHC_GENERICS
import GHC.Generics
#endif

-- | The contravariant analogue of 'Plus'.
--
-- If one thinks of @f a@ as a consumer of @a@s, then 'loss' defines
-- a consumer that cannot ever receive /any/ input.
--
-- Loss acts as an identity with 'decide', because any decision that
-- involves 'loss' must necessarily /always/ pick the other option.
--
-- That is, for, say,
--
-- @
-- 'decide' f x 'lossed'
-- @
--
-- @f@ is the deciding function that picks which of the inputs of @decide@
-- to direct input to; in the situation above, @f@ must /always/ direct all
-- input to @x@, and never 'lossed'.
class Decide f => Loss f where
    -- | The consumer that cannot ever receive /any/ input.
    loss :: (a -> Void) -> f a

-- | A potentially more meaningful form of 'loss', the consumer that cannot
-- ever receive /any/ input.  That is because it expects only input of type
-- 'Void', but such a type has no values.
--
-- @
-- 'lossed' = 'loss' 'id'
-- @
lossed :: Loss f => f Void
lossed = loss id

instance Loss Comparison where loss = lose
instance Loss Equivalence where loss = lose
instance Loss Predicate where loss = lose
instance Loss (Op r) where
  loss f = Op $ absurd . f

#if MIN_VERSION_base(4,8,0)
instance Loss f => Loss (Alt f) where
  loss = Alt . loss
#endif

#ifdef GHC_GENERICS
instance Loss U1 where loss = lose

instance Loss f => Loss (Rec1 f) where
  loss = Rec1 . loss

instance Loss f => Loss (M1 i c f) where
  loss = M1 . loss

instance (Loss f, Loss g) => Loss (f :*: g) where
  loss f = loss f :*: loss f

instance (Applicative f, Loss g) => Loss (f :.: g) where
  loss = Comp1 . pure . loss
#endif

instance Loss f => Loss (Backwards f) where
  loss = Backwards . loss

instance Loss f => Loss (IdentityT f) where
  loss = IdentityT . loss

instance Loss m => Loss (ReaderT r m) where
  loss f = ReaderT $ \_ -> loss f

instance Loss m => Loss (Lazy.RWST r w s m) where
  loss f = Lazy.RWST $ \_ _ -> contramap (\ ~(a, _, _) -> a) (loss f)

instance Loss m => Loss (Strict.RWST r w s m) where
  loss f = Strict.RWST $ \_ _ -> contramap (\(a, _, _) -> a) (loss f)

instance (Divisible m, Divise m) => Loss (ListT m) where
  loss _ = ListT conquer

instance (Divisible m, Divise m) => Loss (MaybeT m) where
  loss _ = MaybeT conquer

instance Loss m => Loss (Lazy.StateT s m) where
  loss f = Lazy.StateT $ \_ -> contramap lazyFst (loss f)

instance Loss m => Loss (Strict.StateT s m) where
  loss f = Strict.StateT $ \_ -> contramap fst (loss f)

instance Loss m => Loss (Lazy.WriterT w m) where
  loss f = Lazy.WriterT $ contramap lazyFst (loss f)

instance Loss m => Loss (Strict.WriterT w m) where
  loss f = Strict.WriterT $ contramap fst (loss f)

instance (Applicative f, Loss g) => Loss (Compose f g) where
  loss = Compose . pure . loss

instance (Loss f, Loss g) => Loss (Product f g) where
  loss f = Pair (loss f) (loss f)

instance Loss f => Loss (Reverse f) where
  loss = Reverse . loss

lazyFst :: (a, b) -> a
lazyFst ~(a, _) = a

