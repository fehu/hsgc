-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Measures
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

-- {-# OPTIONS_GHC -ddump-splices #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module SGC.Object.Measures (

  ObjPosition(..), HasPosition
, readPosition, getPosition, writePosition
, updatePosition, updatePositionValue
, objDistance

, ObjMass(..), HasMass
, readMass, getMass, writeMass
, updateMass, updateMassValue


, MaterialPoint, MaterialPoint'

, module Export

) where

import PhyQ as Export

import SGC.Object as Export
import SGC.Object.Measures.Templates

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

_genMeasurableObjKey "Mass" Mass

-----------------------------------------------------------------------------

-- * Custom keys

-- | Position within a system.
data ObjPosition sys v = ObjPosition sys
instance (Typeable sys, Typeable v) => ObjectKey (ObjPosition sys v) where
  type KeyValue (ObjPosition sys v) = Measurable Position v

type HasPosition obj sys t m v = ObjectHas obj m t '[ObjPosition sys v]

readPosition :: (HasPosition obj sys Any m v) => sys -> obj -> m (Measurable Position v)
readPosition sys = readValue $ ObjPosition sys

getPosition :: (HasPosition obj sys Const m v) => sys -> obj -> Measurable Position v
getPosition sys = getConst $ ObjPosition sys

writePosition :: (HasPosition obj sys Var m v) => sys -> Measurable Position v -> obj -> m ()
writePosition sys = writeVar $ ObjPosition sys

updatePosition :: (HasPosition obj sys Var m v) =>
  sys -> (Measurable Position v -> Measurable Position v) -> obj -> m ()
updatePosition sys = updateVar $ ObjPosition sys

updatePositionValue :: (HasPosition obj sys Var m v) => sys -> (v -> v) -> obj -> m ()
updatePositionValue sys upd = updateVar (ObjPosition sys) (fmap upd)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- * Custom Data and Constraints
type MPoint sys v = '[ ObjMass v, ObjPosition sys v]
type MaterialPoint base m sys v = SomeObject base m '[Has Any (MPoint sys v)]
type MaterialPoint' obj sys m v = ObjectHas obj m Any (MPoint sys v)

-----------------------------------------------------------------------------


-- * Custom functions
objDistance :: ( HasPosition a sys Any m v
               , HasPosition b sys Any m v
               , Num v, Ord v
                ) =>
                sys -> a -> b -> m (Measurable Position v)
objDistance sys x y = do px <- readPosition sys x
                         py <- readPosition sys y
                         return $ py $- px


-----------------------------------------------------------------------------
