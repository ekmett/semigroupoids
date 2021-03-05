{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if __GLASGOW_HASKELL__ == 708
{-# OPTIONS_GHC -fno-warn-amp #-}
#endif

{-|

This module re-exports operators from 'Apply' and 'Bind', but under the same
names their 'Applicative' and 'Monad' counterparts. This makes it convenient
to use do-notation on a type that is a 'Bind' but not a monad (or an 'Apply'
but not an 'Applciative' with @ApplicativeDo@), either using the
@QualifiedDo@ extension or the more traditional @RebindableSyntax@.

@
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QualifiedDo #-}

foo :: Apply f => f a -> f b -> f (a, b)
foo as bs = Semi.do
  a <- as
  b <- bs
  pure (a, b)


bar :: Bind m => (a -> b -> m c) -> m a -> m b -> m c
bar f as bs = Semi.do
  a <- as
  b <- bs
  f a b
@

-}

module Semi
  ( fmap
  , (<*)
  , (*>)
  , (<*>)
  , (>>)
  , (>>=)
  , join
  , pure
  , return
  )
where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure)
#endif
import Data.Functor.Apply (Apply, (<.), (.>), (<.>))
import Data.Functor.Bind (Bind, (>>-), join)
import Prelude hiding
  ( (>>)
  , (>>=)
#if __GLASGOW_HASKELL__ > 708
  , (<*)
  , (*>)
  , (<*>)
#endif
  )


(<*) :: Apply f => f a -> f b -> f a
(<*) = (<.)


(*>) :: Apply f => f a -> f b -> f b
(*>) = (.>)


(<*>) :: Apply f => f (a -> b) -> f a -> f b
(<*>) = (<.>)


(>>) :: Bind m => m a -> m b -> m b
(>>) = (.>)


(>>=) :: Bind m => m a -> (a -> m b) -> m b
(>>=) = (>>-)
