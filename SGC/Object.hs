{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- |
--
-- Module      :  SGC.Object
-- Description :  -
-- License     :  MIT
--
--
--
--


module SGC.Object (

  UniverseConfig(..)
, withUniverseConfig

, Object(..)
, SomeObject(..)

, ObjectForm
, SomeObjectForm(..)

, ObjectSystem(..)
, withObjectSystem


, MassUnit
, PositionUnit

) where


import SGC.Measures
import SGC.Utils

import Data.Typeable

-----------------------------------------------------------------------------

class UniverseConfig c where
    type USystem
    type UNum
    type UVec n
    type UVectorDim

    type ObjectId
    type SystemId

    uSystem :: DoWith c USystem

withUniverseConfig :: (UniverseConfig c) => c -> DoWith c a -> a
withUniverseConfig = doWith

-----------------------------------------------------------------------------

class (Typeable f) => ObjectForm f

data SomeObjectForm = forall f . ObjectForm f => SomeObjForm f

-----------------------------------------------------------------------------


type MassUnit     s = UnitFor s Mass     UScalar
type PositionUnit s = UnitFor s Position UVector


-----------------------------------------------------------------------------


class (UniverseConfig c) =>

    Object a c where

        objId   :: a -> c -> ObjectId
        objMass :: (MassUnit USystem ~ u) => c -> a -> u UNum
        objForm :: a -> c -> SomeObjectForm



data SomeObject c = forall a . (Typeable a, Object a c, Ord a, Show a) => SomeObj c a

instance Show (SomeObject c) where show (SomeObj _ x) = show x

instance Eq (SomeObject c) where
    SomeObj _ x == SomeObj _ y = case cast x of Just x' -> x' == y
                                                _       -> False

instance (Ord ObjectId) => Ord (SomeObject c) where
    SomeObj c x `compare` SomeObj _ y = case cast x of Just x' -> x' `compare` y
                                                       _       -> objId x c `compare` objId y c

class -- (UniverseConfig c) =>

    ObjectSystem s c where
--        type Conf

        systemId      :: DoWith s SystemId
        systemObjects :: DoWith s [SomeObject c]

        positionOf    :: (PositionUnit USystem ~ pos) =>
                       SomeObject c -> DoWith s (Maybe (pos (UVec UNum)))

--        newObjSystem  :: (PositionUnit USystem ~ pos) =>
--                       SystemId -> [(SomeObject c, pos (UVec UNum))] -> DoWith c s


withObjectSystem :: (ObjectSystem s c) => RunDoWith c (RunDoWith s a)
withObjectSystem = doWith

-----------------------------------------------------------------------------





