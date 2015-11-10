{-# LANGUAGE TypeFamilies
           , DataKinds
           , FlexibleInstances
           , ConstraintKinds
         #-}

--
--
-- Module      :  SGC.Measures
-- License     :  MIT
--
--
--


module SGC.Measures (

-- * Physical Quantities
  PhysicalQuantity(..)
, Time(..)
, Mass(..)
, Distance(..), Position(..)
, AbsSpeed(..), Speed(..)

, qMultiply
, qDivide

-- * Units
, UnitSystem(..)
, AbstractUnit(..)
, Unit(..)

, ScalarUnit(..)
, VectorUnit(..)

--, IsVector(..)
, VectorOps(..)

-- * Units ~ Physical Quantities
, QualityOf(..)
, QuantityDimensions(..)
, UnitFor(..)

-- * Units Operations
, UnitsMultiply (uMultiply)
--, uMultiply
--, uDivide
--, ResultDim(..)

---- * Measured Values
--, Measured(..)
----, measure
--, UnitEv
--
---- * 'WithUnitSystem' Monad.
--, WithUnitSystem
--, withUnitSystem


) where


-----------------------------------------------------------------------------

class (Show q) => PhysicalQuantity q where --isScalar :: q -> Bool
                                           --isVector :: q -> Bool

                                           quantityId :: q

                                           --isVector = not . isScalar
                                           --isScalar = not . isVector

data QuantityDimensions = Dimensionless
                        | UScalar
                        | UVector

type family PhQDim q :: QuantityDimensions


data Time     = Time        deriving Show
data Mass     = Mass        deriving Show
data Distance = Distance    deriving Show
data AbsSpeed = AbsSpeed    deriving Show

data Position = Position    deriving Show
data Speed    = Speed       deriving Show



instance PhysicalQuantity Time      where quantityId = Time
type instance PhQDim      Time = UScalar

instance PhysicalQuantity Mass      where quantityId = Mass
type instance PhQDim      Mass = UScalar

instance PhysicalQuantity Distance  where quantityId = Distance
type instance PhQDim      Distance = UScalar

instance PhysicalQuantity AbsSpeed  where quantityId = AbsSpeed
type instance PhQDim      AbsSpeed = UScalar



instance PhysicalQuantity Position  where quantityId = Position
type instance PhQDim      Position = UVector

instance PhysicalQuantity Speed     where quantityId = Speed
type instance PhQDim      Speed = UVector


-----------------------------------------------------------------------------

type family PhQMult a b
type family PhQDiv  a b


qMultiply :: ( PhysicalQuantity a, PhysicalQuantity b
             , PhQMult a b ~ c, PhysicalQuantity c) => a -> b -> c
qMultiply _ _ = quantityId

qDivide   :: ( PhysicalQuantity a, PhysicalQuantity b
             , PhQDiv  a b ~ c, PhysicalQuantity c) => a -> b -> c
qDivide _ _ = quantityId



type instance PhQDiv Position Time = Speed
type instance PhQMult Speed Time = Position

type instance PhQDiv Distance Time = AbsSpeed
type instance PhQMult AbsSpeed Time = Distance

-----------------------------------------------------------------------------

class AbstractUnit u where
    unitValue    :: u n -> n
    createUnit   :: n -> u n

class (Num n, AbstractUnit u) =>
    Unit u s n where
        unitSystem   :: u n -> s
        unitQuantity :: (QualityOf s u ~ q, PhysicalQuantity q) => u n -> q

        unitQuantity _ = quantityId


-- |
class (Fractional n, Unit u s n) =>

    ScalarUnit u s n where

        scalarUnit :: (UnitFor s q UScalar ~ u) => s -> q -> n -> u n
        scalarUnit _ _ = createUnit


--type family IsVector (v :: * -> *) :: Bool

--type family NumVector (v :: *) :: *

-- |
class (Fractional n, Unit u s (v n), VectorOps v n) => -- IsVector v ~ True,

    VectorUnit u s v n where

        vectorUnit :: (UnitFor s q UVector ~ u) => s -> q -> v n -> u (v n)
        vectorUnit _ _ = createUnit

-----------------------------------------------------------------------------

class (Show s) => UnitSystem s where systemId :: s


type family QualityOf s (u :: * -> *) :: *

type family UnitFor s q (dim :: QuantityDimensions) :: * -> *

-----------------------------------------------------------------------------


type family ResultDim a b :: QuantityDimensions where
    ResultDim UScalar UScalar = UScalar
    ResultDim UVector UScalar = UVector
    ResultDim UScalar UVector = UVector
    ResultDim UVector UVector = UVector


type family ResultType a b :: * where
    ResultType n     n     = n
    ResultType (v n) n     = v n
    ResultType n     (v n) = v n


type UnitOpsConstraints s a b c qa qb qc da db dc na nb nc = (
       Unit a s na, QualityOf s a ~ qa, PhQDim qa ~ da
     , Unit b s nb, QualityOf s b ~ qb, PhQDim qb ~ db
     , ResultDim da db ~ dc
     , ResultType na nb ~ nc
     , PhQDim qc ~ dc
     , UnitFor s qc dc ~ c, Unit c s nc
     )


--class UnitsMultiply s na da nb db where -- nc dc
--    uMultiply :: ( PhQMult qa qb ~ qc
--                 , UnitOpsConstraints s a b c qa qb qc da db dc na nb nc) =>
--          s -> a na -> b nb -> c nc



class UnitsMultiply s na da nb db nc where -- nc dc
    uMultiply :: ( PhQMult qa qb ~ qc
                 , UnitOpsConstraints s a b c qa qb qc da db dc na nb nc) =>
          s -> a na -> b nb -> c nc


instance
    UnitsMultiply s n UScalar n UScalar n where
        uMultiply _ x y = createUnit $ unitValue x * unitValue y


class VectorOps v n where vMultiplyConst' :: v n -> n -> v n

instance (VectorOps v n) =>
    UnitsMultiply s (v n) UVector n UScalar (v n) where
        uMultiply _ x y = createUnit $ vMultiplyConst' (unitValue x)
                                                       (unitValue y)


-----------------------------------------------------------------------------
-- TODO: Tests

absSpeed s = scalarUnit s AbsSpeed (5 :: Float)
time s = scalarUnit s Time (10 :: Float)
dist s = uMultiply s (absSpeed s) (time s)


data Vec2 n = Vec2 n n deriving Show

--type instance IsVector Vec2 = True
instance Num (Vec2 n)
instance (Fractional n) =>
    VectorOps Vec2 n where vMultiplyConst' (Vec2 x y) c = Vec2 (c*x) (c*y)

--instance Num (Vec2 n)

speed s = vectorUnit s Speed (Vec2 2.3 5.2 :: Vec2 Float)
dist2 s = uMultiply s (speed s) (time s)



