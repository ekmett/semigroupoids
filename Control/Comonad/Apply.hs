{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Apply
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Comonad' is the categorical dual of a 'Monad'.
----------------------------------------------------------------------------
module Control.Comonad.Apply ( 
  -- * FunctorApply
    module Control.Comonad
  , module Data.Functor.Apply
  -- * ComonadApply - strong lax symmetric semimonoidal comonads
  , ComonadApply
  , liftW2    -- :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
  , liftW3    -- :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  ) where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Monad.Trans.Identity
import Data.Functor.Apply
import Data.Functor.Identity
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup(..))

{- | 

A strong lax symmetric semi-monoidal comonad. As such, an instance of 
'ComonadApply' is required to satisfy:

> extract (a <.> b) = extract a (extract b)

This class is based on ComonadZip from \"The Essence of Dataflow Programming\" 
by Tarmo Uustalu and Varmo Vene, but adapted to fit the programming style of
Control.Applicative. 'Applicative' can be seen as a similar law over and above 
FunctorApply that:

> pure (a b) = pure a <.> pure b

-}

class (Comonad w, Apply w) => ComonadApply w
-- | Both required because Semigroup is not a superclass of Monoid
instance (Monoid m, Semigroup m) => ComonadApply ((,)m)
instance (Monoid m, Semigroup m)  => ComonadApply ((->)m)
instance ComonadApply Identity
instance ComonadApply w => ComonadApply (IdentityT w)
instance ComonadApply w => ComonadApply (MaybeApply w)

-- | Lift a binary function into a comonad with zipping
liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW2 = liftF2
{-# INLINE liftW2 #-}

-- | Lift a ternary function into a comonad with zipping
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 = liftF3
{-# INLINE liftW3 #-}

-- | A sad little orphan
instance ComonadApply w => ArrowLoop (Cokleisli w) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where 
    f' wa wb = f ((,) <$> wa <.> (snd <$> wb))
