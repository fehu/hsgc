{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , ExistentialQuantification
           , ConstraintKinds
           , FlexibleInstances
--           , UndecidableInstances
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.NewMeasures
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module SGC.NewMeasures (

-- * Physical Quantities
  PhysicalQuantity(..)

-- * Units
, UnitSystem(..)
, Unit(..)
, UnitFor(..)

-- * Measured Values
, UnitEv

-- * 'WithUnitSystem' Monad.
, WithUnitSystem
, withUnitSystem



--, baz

) where


import Control.Applicative

-----------------------------------------------------------------------------

class (Show q) => PhysicalQuantity q where isScalar :: q -> Bool
                                           isVector :: q -> Bool

                                           quantityId :: q

                                           isVector = not . isScalar
                                           isScalar = not . isVector

-----------------------------------------------------------------------------

class (Show s) =>
    UnitSystem s where
        type UnitPrefix
        unitSystemId :: s

        noPrefix :: s -> UnitPrefix


class (Show (u v), UnitSystem s) =>
    Unit u s v where unitSystem   :: u v -> s
                     unitQuantity :: (PhysicalQuantity q, UnitFor q s ~ u) => u n -> q
                     unitValue    :: u v -> v
                     mkUnit       :: n -> s -> u n

                     unitSystem _   = unitSystemId
                     unitQuantity _ = quantityId


type family UnitFor q s :: * -> *

-----------------------------------------------------------------------------



newtype (UnitSystem s) =>
    WithUnitSystem s r = WithUnitSystem (s -> r)



instance (UnitSystem s) =>
    Functor (WithUnitSystem s) where
        fmap f (WithUnitSystem g) = WithUnitSystem (f . g)

instance (UnitSystem s) =>
    Applicative (WithUnitSystem s) where
        pure = WithUnitSystem . const
        WithUnitSystem f <*> WithUnitSystem g = WithUnitSystem $ \s -> f s $ g s

instance (UnitSystem s) =>
    Monad (WithUnitSystem s) where
        return = pure
        WithUnitSystem g >>= f = WithUnitSystem $
            \s -> case f (g s) of WithUnitSystem g' -> g' s


mapwus2 f x y = do x' <- x
                   y' <- y
                   return $ f x' y'

instance (UnitSystem s, Num r) => Num (WithUnitSystem s r) where
    (+) = mapwus2 (+)
    (*) = mapwus2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = pure . fromInteger

-----------------------------------------------------------------------------

withUnitSystem :: (UnitSystem s)    => s -> WithUnitSystem s r -> r
withUnitSystem s (WithUnitSystem f) = f s

withUnitSystem' (WithUnitSystem f) = f

type UnitEv q s u v = (UnitFor q s ~ u, UnitSystem s, Unit u s v)

--mapU :: (UnitEv q s u v) =>
--mapU f u = undefined

-----------------------------------------------------------------------------





