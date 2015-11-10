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

-- * Units ~ Physical Quantities
, QualityOf(..)
, QuantityDimensions(..)
, UnitFor(..)

-- * Units Operations
, uMultiply
, uDivide
, ResultDim(..)

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

class (Show q) => PhysicalQuantity q where isScalar :: q -> Bool
                                           isVector :: q -> Bool

                                           quantityId :: q

                                           isVector = not . isScalar
                                           isScalar = not . isVector

data Time     = Time        deriving Show
data Mass     = Mass        deriving Show
data Distance = Distance    deriving Show
data AbsSpeed = AbsSpeed    deriving Show

data Position = Position    deriving Show
data Speed    = Speed       deriving Show

instance PhysicalQuantity Time      where isScalar _ = True
                                          quantityId = Time
instance PhysicalQuantity Mass      where isScalar _ = True
                                          quantityId = Mass
instance PhysicalQuantity Distance  where isScalar _ = True
                                          quantityId = Distance
instance PhysicalQuantity AbsSpeed  where isScalar _ = True
                                          quantityId = AbsSpeed

instance PhysicalQuantity Position  where isVector _ = True
                                          quantityId = Position
instance PhysicalQuantity Speed     where isVector _ = True
                                          quantityId = Speed


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

class (Fractional n, Unit u s n) =>
    ScalarUnit u s n where
        scalarUnit :: (UnitFor s q Scalar ~ u) => s -> q -> n -> u n
        scalarUnit _ _ = createUnit


-----------------------------------------------------------------------------

class (Show s) => UnitSystem s where systemId :: s

data QuantityDimensions = Dimensionless
                        | Scalar
                        | Vector


type family QualityOf s (u :: * -> *) :: *

type family UnitFor s q (dim :: QuantityDimensions) :: * -> *

-----------------------------------------------------------------------------


type family ResultDim a b :: QuantityDimensions where
    ResultDim a b = Scalar


type UnitOpsConstraints s a b c qa qb qc v n = (
       Unit a s n, QualityOf s a ~ qa
     , Unit b s n, QualityOf s b ~ qb
     , ResultDim qa qb ~ v
     , UnitFor s qc v ~ c, Unit c s n
     )


uMultiply :: (PhQMult qa qb ~ qc, UnitOpsConstraints s a b c qa qb qc v n) =>
          s -> a n -> b n -> c n
uMultiply _ x y = createUnit $ unitValue x * unitValue y


uDivide :: (PhQDiv qa qb ~ qc, UnitOpsConstraints s a b c qa qb qc v n) =>
        s -> a n -> b n -> c n
uDivide _ x y = createUnit $ unitValue x * unitValue y









-----------------------------------------------------------------------------
-- TODO: Tests

absSpeed s = scalarUnit s AbsSpeed (5 :: Float)
time s = scalarUnit s Time (10 :: Float)
dist s = uMultiply s (absSpeed s) (time s)







