{-# LANGUAGE Safe #-}

{-|

This module re-exports operators from "Data.Functor.Apply" and
"Data.Functor.Bind", but under the same
names as their 'Applicative' and 'Monad' counterparts. This makes it convenient
to use do-notation on a type that is a 'Bind' but not a monad (or an 'Apply'
but not an 'Applicative' with @ApplicativeDo@), either using the
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
module Semigroupoids.Do
  ( fmap
  , (<*)
  , (*>)
  , (<*>)
  , (>>)
  , (>>=)
  , join
  , pure
  , return
  , fail
  )
where

import Prelude (String, fmap, pure, return)
import Data.Functor.Apply (Apply, (<.), (.>), (<.>))
import Data.Functor.Bind (Bind, (>>-), join)
import Data.Functor.Plus (Plus, zero)

-- | @since 5.3.6
(<*) :: Apply f => f a -> f b -> f a
(<*) = (<.)

-- | @since 5.3.6
(*>) :: Apply f => f a -> f b -> f b
(*>) = (.>)

-- | @since 5.3.6
(<*>) :: Apply f => f (a -> b) -> f a -> f b
(<*>) = (<.>)

-- | @since 5.3.6
(>>) :: Bind m => m a -> m b -> m b
(>>) = (.>)

-- | @since 5.3.6
(>>=) :: Bind m => m a -> (a -> m b) -> m b
(>>=) = (>>-)

-- | = Important note
--
-- This /ignores/ whatever 'String' you give it. It is a bad idea to use 'fail'
-- as a form of labelled error; instead, it should only be defaulted to when a
-- pattern match fails.
--
-- @since 5.3.6
fail ::
  (Plus m) =>
  String ->
  m a
fail _ = zero
