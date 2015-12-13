{-# LANGUAGE ExistentialQuantification #-}

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
, SomeObjSys(..)
, CoordinateSystem(..)


) where

import SGC.Coordinates
import SGC.Object

import Data.Typeable

-----------------------------------------------------------------------------

class Universe u where
    uRootSystem :: u -> SomeObjSys


-----------------------------------------------------------------------------

data SomeObjSys = forall a . (ObjectSystem a, Typeable a) => SomeObjSys a

class (Object sys) => ObjectSystem sys where
    systemObjs :: sys -> [SomeObj]


-----------------------------------------------------------------------------

data Interaction usys num = Interaction{
--    interactionForce :: (UnitFor usys ~ force) => force num
}


class InteractionLaw i usys num where
    objsInteraction :: i -> SomeObj -> SomeObj -> Maybe (Interaction usys num)


