{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Form
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

module SGC.Object.Form (

  ObjectForm(..)
, SimpleForm(..)
, CustomForm(..)

) where

import SGC.Measures

import Data.Typeable

-----------------------------------------------------------------------------


data ObjectForm usys num = SimpleForm (SimpleForm usys num)
                         | forall f . (CustomForm f usys num, Typeable f) => CustomForm f


class CustomForm f usys num where
--    customFormName :: f -> String

-----------------------------------------------------------------------------

data SimpleForm usys num = FormPoint
                         |  forall u . (PositionUnit usys ~ u) =>
                           FormSphere { fSphereRadius :: u num }

