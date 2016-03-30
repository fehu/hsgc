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
    type UnitStructure Second = '[ '(Second, Pos 1) ]
    type UnitHash Second = 0



data Meter = Meter
instance Unit Meter where unitName _ = "m"
                          unitInstance = Meter
instance UnitDecomposition Meter where
    type UnitStructure Meter = '[ '(Meter, Pos 1) ]
    type UnitHash Meter = 1


data Kilogramm = Kilogramm
instance Unit Kilogramm where unitName _ = "kg"
                              unitInstance = Kilogramm
instance UnitDecomposition Kilogramm where
    type UnitStructure Kilogramm = '[ '(Kilogramm, Pos 1) ]
    type UnitHash Kilogramm = 2




data MeterPerSecond = MeterPerSecond
instance Unit MeterPerSecond where unitName _ = "m/s"
                                   unitInstance = MeterPerSecond
instance UnitDecomposition MeterPerSecond where
    type UnitStructure MeterPerSecond = '[ '(Meter, Pos 1) , '(Second, Neg 1) ]


data Newton = Newton
instance Unit Newton where unitName _ = "N"
                           unitInstance = Newton
instance UnitDecomposition Newton where
    type UnitStructure Newton = UnitStructure (Kilogramm * Meter / (Second ^ Int' (Pos 2)))



-----------------------------------------------------------------------------


bb :: ((Succ Zero == Succ Zero) ~ True, (Succ Zero == Prev Zero) ~ False) => Bool
bb = undefined


x :: (Second !== x) => x
x = undefined


--xx :: (Newton !== x) => x -> Int
--xx = undefined

aa :: Meter / Second
aa = undefined

--aa' :: Kilogramm * Meter / (Second ^ Int' (Pos 2)) -- / Second / Second
--aa' = undefined

--xx' = xx aa' -- (Meter :/ Second)


-----------------------------------------------------------------------------

data AA = AA
instance Unit AA where unitName _ = "A"
                       unitInstance = AA
instance UnitDecomposition AA where type UnitStructure AA = UnitStructure (Meter * Second)


qq :: (AA !== x) => x -> Int
qq = undefined

qq' = qq (Second :* Meter)

qq'' = qq (Meter :* Second)

--qq''' = qq (Meter :* Kilogramm)

-----------------------------------------------------------------------------


ww :: ((Meter * Second) !== (Second * Meter)) => x
ww = undefined
--
--ww' :: ((Meter * Second) !== (Second * Kilogramm)) => x
--ww' = undefined



