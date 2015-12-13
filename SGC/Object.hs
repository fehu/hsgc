{-# LANGUAGE ExistentialQuantification #-}

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
, SomeObj(..)

, InCoordinateSystem(..)
, HasMass(..)
, HasForm(..)

) where

import SGC.Coordinates
import SGC.Measures
import SGC.Object.Form

import Data.Typeable

-----------------------------------------------------------------------------

class Object a where
    type Id a :: *
    objId :: a -> Id a


data SomeObj = forall a . (Object a, Typeable a) => SomeObj a

--instance (Object a) => Eq a where


-----------------------------------------------------------------------------

class HasMass a usys num where
    objMass :: (MassUnit usys ~ mass) => a num -> mass num


class HasForm a usys num where
    objForm :: a -> ObjectForm usys num



-----------------------------------------------------------------------------




