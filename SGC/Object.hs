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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module SGC.Object (


) where

import SGC.Object.Generic --  as Export
import SGC.Object.Generic.TypeMap

import PhyQ

import Data.Typeable (Typeable)
import Data.Type.Bool
import Data.Type.Equality

import GHC.Exts (Constraint)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data MeasurableKey q v = MeasurableKey
measurableKey :: q -> MeasurableKey q v
measurableKey _ = MeasurableKey



type HasMeasures obj (ms :: [*]) m v = MergeConstraints (ValueForEach obj ms m v)
-- data ObjectMeasures   (ms :: [*]) m v =
--   forall cs vs . HasMeasures (Object cs vs) ms m v =>
--     ObjectMeasures (Object cs vs)
--
-- withMeasures :: (forall obj . HasMeasures obj ms m v => obj -> r)
--              -> ObjectMeasures ms m v
--              -> r
-- withMeasures f (ObjectMeasures obj) = f obj




-----------------------------------------------------------------------------

instance (Typeable q, Typeable v) =>
  TypeMapKey (MeasurableKey q v) where
    type TMValue (MeasurableKey q v) = Measurable q v




type family MergeConstraints (cs :: [Constraint]) :: Constraint
  where  MergeConstraints cs = MergeConstraints' cs ()

type family MergeConstraints' (cs :: [Constraint]) (acc :: Constraint) :: Constraint
  where MergeConstraints' (c ': cs) acc = (c, acc)
        MergeConstraints' '[] acc = acc

type family ValueForEach obj (ms :: [*]) rm v :: [Constraint]
  where ValueForEach obj (m ': ms) rm v = ObjectValue obj (MeasurableKey m v) rm
                                        ': ValueForEach obj ms rm v
        ValueForEach obj '[] rm v = '[]

-----------------------------------------------------------------------------

-- type Measure a m v q = a -> m (Measurable q v)
--
-- class HasMass v m a where
--   objMass :: Measure a m v Mass
--
-- instance ( Typeable m, Typeable n, Monad m, TypeMap cs, TypeMap vs
--          , ObjectValue (Object cs vs) (MeasurableKey Mass n) m
--           ) =>
--   HasMass n m (Object cs vs) where
--     objMass obj = readValue obj (measurableKey Mass)

-----------------------------------------------------------------------------

-- getMass :: (Typeable v) => ObjectMeasures '[Mass] m v -> m (Measurable Mass v)
-- getMass = withMeasures (`readValue` measurableKey Mass)

-----------------------------------------------------------------------------
