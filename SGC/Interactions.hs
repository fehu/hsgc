-----------------------------------------------------------------------------
--
-- Module      :  SGC.Interactions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeOperators #-}

module SGC.Interactions where

import SGC.Universe
import SGC.Object.Measures

import PhyQ
import TypeNum.Rational hiding (Abs)

-----------------------------------------------------------------------------

class Gravity v where
  gravityConst :: Measurable ((Distance:^3) :* (Time:^Neg 2) :/ Mass) v

  gravity :: (MeasurableVector vec v, Ord vec, Num vec, Ord v, Fractional v) =>
             Interaction usys vec sys id m -- (MaterialPoint NoCtx m sys v) --  (MaterialPoint sys  (vec v))
  -- gravity = Interaction $
  --   \sys x y -> let dist    = objDistance sys x y
  --                   distAbs = measurableAbs dist
  --                   asbF  = gravityConst $* objMass x $* objMass y
  --                                        $/ (distAbs $* distAbs)
  --                   dist1 = measurable' (quantityInstance :: Distance) 1
  --               in measureCoerce $
  --                   measurableScalarMult (measurableNorm dist) (asbF $/ dist1)








-----------------------------------------------------------------------------
