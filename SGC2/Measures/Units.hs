-----------------------------------------------------------------------------
--
-- Module      :  SGC2.Measures.Units
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE UndecidableInstances
           , TypeOperators -- TODO TMP
       #-}


module SGC2.Measures.Units where


import SGC2.Measures.Definitions
import SGC2.Measures.Decomposition

import TypeNum.Integer

import Data.Type.Equality -- TODO: TMP

-----------------------------------------------------------------------------

data Second = Second
instance Unit Second where unitName _ = "s"
                           unitInstance = Second
instance UnitDecomposition Second where
    type UnitStructure' Second = '[ '(Second, Pos 1) ]



data Meter = Meter
instance Unit Meter where unitName _ = "m"
                          unitInstance = Meter
instance UnitDecomposition Meter where
    type UnitStructure' Meter = '[ '(Meter, Pos 1) ]



data MeterPerSecond = MeterPerSecond
instance Unit MeterPerSecond where unitName _ = "m/s"
                                   unitInstance = MeterPerSecond
instance UnitDecomposition MeterPerSecond where
    type UnitStructure' MeterPerSecond = '[ '(Meter, Pos 1) , '(Second, Neg 1) ]







-----------------------------------------------------------------------------


bb :: ((Succ Zero == Succ Zero) ~ True, (Succ Zero == Prev Zero) ~ False) => Bool
bb = undefined


x :: (Second !== x) => x
x = undefined


xx :: (MeterPerSecond !== x) => x -> Int
xx = undefined

aa :: Meter / Second
aa = undefined

xx' = xx aa -- (Meter :/ Second)


