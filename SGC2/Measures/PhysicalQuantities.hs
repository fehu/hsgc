-----------------------------------------------------------------------------
--
-- Module      :  SGC2.Measures.PhysicalQuantities
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module SGC2.Measures.PhysicalQuantities where


import SGC2.Measures.Definitions


-----------------------------------------------------------------------------

-- * Dimensionless
$(genPhysicalQuantity "Dimensionless" Dimensionless)


-- * Scalar
$(genPhysicalQuantity "Time" Scalar)
$(genPhysicalQuantity "Mass" Scalar)
$(genPhysicalQuantity "Distance" Scalar)
type Length = Distance
$(genPhysicalQuantity "Temperature" Scalar)
$(genPhysicalQuantity "PlaneAngle" Scalar)
type Angle = PlaneAngle
$(genPhysicalQuantity "SolidAngle" Scalar)


-- * Vector
$(genPhysicalQuantity "Position" Vector)
$(genPhysicalQuantity "Speed" Vector)
$(genPhysicalQuantity "Acceleration" Vector)
$(genPhysicalQuantity "Force" Vector)
$(genPhysicalQuantity "Impulse" Vector)




