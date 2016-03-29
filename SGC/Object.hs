{-# LANGUAGE ExistentialQuantification
           , FlexibleInstances
           , DeriveDataTypeable
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module SGC.Object (

  Object(..)
, SObject(..)

--, ObjContainer(..)
, SomeObj(..)
--, PhysicalObj(..)

, InCoordinateSystem(..)
, HasMass(..)
, HasForm(..)

, PhysicalObject(..)

) where

import SGC.Coordinates
import SGC.Measures
import SGC.Object.Form

import Data.Typeable

-----------------------------------------------------------------------------

class Object a where
    type Id a :: *
    objId :: a -> Id a

--instance (Object a) => Eq a where

-- | Object with an assosiated UnitSystem.
class (Object a) =>
    SObject a usys num

-----------------------------------------------------------------------------

data SomeObj usys num = forall a . (SObject a usys num, Typeable a) =>
     SomeObj a deriving Typeable

--data PhysicalObj usys num = forall a . ( HasMass a usys num
--                                       , HasForm a usys num
--                                       , Typeable a)        =>
--     PhysicalObj a deriving Typeable


--data ObjContainer = forall c . Typeable c => ObjContainer c

--class Typeable (c usys num) =>
--    ObjectContainer (c :: * -> * -> *) usys num
--data ObjContainer usys num = forall c . ( ObjectContainer c usys num ) => -- , Typeable (c usys num)
--     ObjContainer (c usys num)

--instance (Typeable num, Typeable usys) => ObjectContainer SomeObj     usys num
--instance (Typeable num, Typeable usys) => ObjectContainer PhysicalObj usys num

-----------------------------------------------------------------------------

class (SObject a usys num) =>
    HasMass a usys num where
        objMass :: (MassUnit usys ~ mass) => a -> mass num


class (SObject a usys num) =>
    HasForm a usys num where
        objForm :: a -> ObjectForm usys num



-----------------------------------------------------------------------------

class (HasMass a usys num, HasForm a usys num, InCoordinateSystem a sys usys num) =>
    PhysicalObject a sys usys num

-----------------------------------------------------------------------------




