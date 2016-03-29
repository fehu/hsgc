{-# LANGUAGE ExistentialQuantification, FlexibleInstances, RankNTypes #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.Universe
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module SGC.Universe (

  Universe(..)

, ObjectSystem(..)
--, SomeObjSys(..)
, CoordinateSystem(..)

, IdealInteraction(..)
--, IdealInteractionLaw(..)

) where

import SGC.Measures
import SGC.Coordinates
import SGC.Object

import Data.Typeable
import Data.Set
import GHC.Exts

-----------------------------------------------------------------------------

class Universe u where
--    uRootSystem :: u -> SomeObjSys


-----------------------------------------------------------------------------

class (SObject (sys usys num) usys num) =>
    ObjectSystem sys usys num c where
        systemObjs :: sys usys num -> [SomeObj usys num]
--        systemLaws :: Set (SomeIdealInteractionLaw (sys usys num) usys num c)

-----------------------------------------------------------------------------

--data SomeObjSys = forall a usys num c . (ObjectSystem a usys num c, Typeable a) =>
--     SomeObjSys (a usys num c)



-----------------------------------------------------------------------------

-- | An /ideal/ interraction, with no deformation of the participating objects.
data IdealInteraction usys num = forall force impulse . ( ForceUnit usys ~ force
                                                        , ImpulseUnit usys ~ impulse) =>
    IdealInteraction{ interactionForce    :: force num
                    , interactionImpulse  :: impulse num
                    }


type IdealInteractionLaw (c :: * -> * -> * -> Constraint) usys num =
        (c a usys num, c b usys num)
                    => a
                    -> b
                    -> Maybe (IdealInteraction usys num)

--class IdealInteractionLaw i sys usys num (c :: * -> * -> * -> *) where
--    objsInteraction :: (c a sys usys num, c sys b usys num) => i
--                    -> a
--                    -> b
--                    -> Maybe (IdealInteraction usys num)

--data SomeIdealInteractionLaw sys usys num c = forall l . IdealInteractionLaw l sys usys num c =>
--     SomeIdealInteractionLaw l

-----------------------------------------------------------------------------

newtype Gravity num = Gravity num

gravityInteraction :: Gravity num
                   -> sys
                   -> IdealInteractionLaw (PhysicalObject sys) usys num

gravityInteraction (Gravity g) sys a b = do
    posX <- coordinateOf sys a
    posY <- coordinateOf sys b
    let x = g * objMass a * objMass b / (posX * posY)
    undefined


--instance IdealInteractionLaw (Gravity num) usys num PhysicalObject where
--    objsInteraction _ (ObjContainer o1') (ObjContainer o2') =
--        case cast o1' of Just (PhysicalObj o1) -> undefined
--    do
--        PhysicalObj o1 <- cast o1' -- :: (Maybe (PhysicalObj usys num))
--        let x = objMass o1
--        undefined
--        where x = (objMass o1) * (objMass o2)














