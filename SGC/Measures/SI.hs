{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}


--
--
-- Module      :  SGC.Measures.SI
-- License     :  MIT
--
--
--
--


module SGC.Measures.SI (

  SI(..)

, Second (Sec)
, Kilogramm (Kg)
, Meter (M)
, MeterPerSecond (Mps)

) where

import SGC.Measures

-----------------------------------------------------------------------------

data SI = SI deriving Show

instance UnitSystem SI where systemId = SI

--data SIPrefix = Tera
--              | Mega
--              | Giga
--              | Kilo
--              | SINoPreifx
--              | Milli
--              | Micro
--              | Nano
--              | Pico


-----------------------------------------------------------------------------

data Second    n        = Sec n deriving (Show, Eq, Ord)
data Kilogramm n        = Kg  n deriving (Show, Eq, Ord)
data Meter     n        = M   n deriving (Show, Eq, Ord)
data MeterPerSecond n   = Mps n deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------


newtype Vec u n = Vec (u n)


instance (AbstractUnit u)               => AbstractUnit (Vec u) where
    unitValue (Vec u) = unitValue u
    createUnit        = Vec . createUnit
instance (Unit u s n)                   => Unit (Vec u) s n where
    unitSystem (Vec u) = unitSystem u
--instance (VectorUnit u s n)             => VectorUnit (Vec u) s n



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------



-- Second
-----------------------------------------------------------------------------
instance (Fractional n) => Unit Second SI n where unitSystem _ = SI
instance (Fractional n) => ScalarUnit Second SI n

type instance QualityOf SI Second = Time
type instance UnitFor SI Time Scalar = Second

instance AbstractUnit Second where unitValue (Sec x) = x
                                   createUnit = Sec

-- Kilogramm
-----------------------------------------------------------------------------
instance (Fractional n) => Unit Kilogramm SI n where unitSystem _ = SI
instance (Fractional n) => ScalarUnit Kilogramm SI n

type instance QualityOf SI Kilogramm = Mass
type instance UnitFor SI Mass Scalar = Kilogramm

instance AbstractUnit Kilogramm where unitValue (Kg x) = x
                                      createUnit = Kg

-- Meter
-----------------------------------------------------------------------------
instance (Fractional n) => Unit Meter SI n where unitSystem _ = SI
instance (Fractional n) => ScalarUnit Meter SI n
--instance VectorUnit MeterPerSecond

type instance QualityOf SI Meter         = Distance
type instance QualityOf SI (Vec Meter)   = Position
type instance UnitFor SI Distance Scalar = Meter
type instance UnitFor SI Position Vector = Vec Meter

instance AbstractUnit Meter where unitValue (M x) = x
                                  createUnit = M

-- MeterPerSecond
-----------------------------------------------------------------------------
instance (Fractional n) => Unit MeterPerSecond SI n where unitSystem _ = SI
instance (Fractional n) => ScalarUnit MeterPerSecond SI n
--instance VectorUnit MeterPerSecond
type instance QualityOf SI MeterPerSecond       = AbsSpeed
type instance QualityOf SI (Vec MeterPerSecond) = Speed
type instance UnitFor SI AbsSpeed Scalar = MeterPerSecond
type instance UnitFor SI Speed    Vector = Vec MeterPerSecond
instance AbstractUnit MeterPerSecond where unitValue (Mps x) = x
                                           createUnit = Mps







-----------------------------------------------------------------------------
-- TODO: Tests

absSpeed = scalarUnit SI AbsSpeed (5 :: Float)
time = scalarUnit SI Time (10 :: Float)

dist :: Meter Float
dist = uMultiply SI absSpeed time




