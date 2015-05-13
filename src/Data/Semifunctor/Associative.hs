{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, GADTs
--
----------------------------------------------------------------------------
module Data.Semifunctor.Associative where

import Prelude hiding ((.), id)
import Control.Arrow
import Data.Functor.Bind
import Data.Semifunctor
-- import Data.Isomorphism

#ifdef MIN_VERSION_comonad
import Data.Functor.Extend
import Control.Comonad
#endif

class Semifunctor p (Product k k) k => Associative k p where
  associate :: k (p '(p '(a,b) ,c)) (p '(a, p '(b,c)))

instance Associative (->) (Bi Either) where
  associate (Bi (Left (Bi (Left a)))) = Bi (Left a)
  associate (Bi (Left (Bi (Right b)))) = Bi (Right (Bi (Left b)))
  associate (Bi (Right c)) = Bi (Right (Bi (Right c)))

instance Associative (->) (Bi (,)) where
  associate (Bi (Bi (a,b),c)) = Bi(a, Bi(b, c))

kleisliAssociate :: (Monad m, Semifunctor p (Product (Kleisli m) (Kleisli m)) (Kleisli m), Associative (->) p) => Kleisli m (p '(p '(a,b),c)) (p '(a,p '(b,c)))
kleisliAssociate = Kleisli (return . associate)

instance (Bind m, Monad m) => Associative (Kleisli m) (Bi Either) where
  associate = kleisliAssociate

instance (Bind m, Monad m) => Associative (Kleisli m) (Bi (,)) where
  associate = kleisliAssociate

#ifdef MIN_VERSION_comonad
cokleisliAssociate :: (Comonad m, Semifunctor p (Product (Cokleisli m) (Cokleisli m)) (Cokleisli m), Associative (->) p) => Cokleisli m (p '(p '(a,b),c)) (p '(a,p '(b,c)))
cokleisliAssociate = Cokleisli (associate . extract)

instance (Extend m, Comonad m) => Associative (Cokleisli m) (Bi (,)) where
  associate = cokleisliAssociate

-- instance Comonad m => Associative (Cokleisli m) (Bi Either) where associate = cokleisliAssociate
#endif

-- instance Disassociative k p => Associative (Dual k) p
-- instance (Monad m, Semifunctor p (Product (Kleisli m) (Kleisli m) (Kleisli m), Associative (->) p) => Associative (Kleisli m) p) where associate = kleisliAssociate

class Semifunctor p (Product k k) k => Disassociative k p where
  disassociate :: k (p '(a,p '(b,c))) (p '(p '(a,b),c))

instance Disassociative (->) (Bi Either) where
  disassociate (Bi (Left a)) = Bi (Left (Bi (Left a)))
  disassociate (Bi (Right (Bi (Left b)))) = Bi (Left (Bi (Right b)))
  disassociate (Bi (Right (Bi (Right b)))) = Bi (Right b)

instance Disassociative (->) (Bi (,)) where
  disassociate (Bi(a, Bi(b, c))) = Bi (Bi (a,b),c)

kleisliDisassociate :: (Monad m, Semifunctor p (Product (Kleisli m) (Kleisli m)) (Kleisli m), Disassociative (->) p) => Kleisli m (p '(a,p '(b,c))) (p '(p '(a,b),c))
kleisliDisassociate = Kleisli (return . disassociate)

instance (Bind m, Monad m) => Disassociative (Kleisli m) (Bi Either) where
  disassociate = kleisliDisassociate

instance (Bind m, Monad m) => Disassociative (Kleisli m) (Bi (,)) where
  disassociate = kleisliDisassociate

#ifdef MIN_VERSION_comonad
cokleisliDisassociate :: (Comonad m, Semifunctor p (Product (Cokleisli m) (Cokleisli m)) (Cokleisli m), Disassociative (->) p) => Cokleisli m (p '(a,p '(b,c))) (p '(p '(a,b),c))
cokleisliDisassociate = Cokleisli (disassociate . extract)

instance (Extend m, Comonad m) => Disassociative (Cokleisli m) (Bi (,)) where
  disassociate = cokleisliDisassociate
#endif

--  instance Associative k p => Disassociative (Dual k) p

-- instance (Associative k p, Disassociative k p) => Associative (Iso k) p where
--  associate = Iso associate disassociate

--instance (Associative k p, Disassociative k p) => Disassociative (Iso k) p where
--  disassociate = Iso disassociate associate
