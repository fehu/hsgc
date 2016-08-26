-----------------------------------------------------------------------------
--
-- Module      :  SGC2.Measures.Decomposition
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE TypeOperators
           , UndecidableInstances
           , PolyKinds
       #-}

module SGC2.Measures.Decomposition where

import SGC2.Measures.Definitions

import Data.Type.Equality
import Data.Type.Bool

import GHC.TypeLits (Nat, CmpNat)
import TypeNum.Integer
import TypeNum.Rational


-----------------------------------------------------------------------------

class (Unit u) => UnitDecomposition u where type UnitStructure u :: [(*, TInt)]
                                            type UnitHash      u :: Nat


-----------------------------------------------------------------------------

type family (++) (l1 :: [(*, TInt)]) (l2 :: [(*, TInt)]) :: [(*, TInt)] where
    '[]      ++ l = l
    (h ': t) ++ l = h ': (t ++ l)


type family NegatePowers (l :: [(*, TInt)]) :: [(*, TInt)] where
    NegatePowers '[] = '[]
    NegatePowers ('(u, x) ': t) = '(u, Negate x) ': NegatePowers t


-----------------------------------------------------------------------------


type family DecompositionEq (a :: [(*, TInt)]) (b :: [(*, TInt)]) :: Bool where
    DecompositionEq '[] '[] = True
    DecompositionEq ('(ua, ia) ': ta) ('(ub, ib) ': tb) = ua == ub &&
                                                          ia == ib &&
                                                          DecompositionEq ta tb

-----------------------------------------------------------------------------

type family Compare a b :: Ordering where
    Compare a b = CmpNat (UnitHash a) (UnitHash b)


type family FilterOrd' (ord :: Ordering) (e :: k) (l :: [(k, TInt)]) :: [(k, TInt)] where
    FilterOrd' ord e '[] = '[]
    FilterOrd' ord e ('(u, n) ': t) = If (Compare e u == ord ) ('(u, n) ': FilterOrd' ord e t)
                                                               (FilterOrd' ord e t)


type family TQSort' (l :: [(*, TInt)]) :: [(*, TInt)] where
    TQSort' '[] = '[]
    TQSort' ('(u, n) ': t) = TQSort' (FilterOrd' LT u t)
                          ++ '[ '(u, n) ]
                          ++ TQSort' (FilterOrd' GT u t)

type family DecompositionEq' (a :: [(*, TInt)]) (b :: [(*, TInt)]) :: Bool where
    DecompositionEq' '[] '[] = True
    DecompositionEq' as bs   = DecompositionEq (TQSort' as) (TQSort' bs)

-----------------------------------------------------------------------------

type (!==) a b = DecompositionEq' (UnitStructure a) (UnitStructure b) ~ True

-----------------------------------------------------------------------------

instance (Unit a, Unit b) => UnitDecomposition (a :* b) where
    type UnitStructure (a :* b) = UnitStructure a  ++ UnitStructure b
instance (Unit a, Unit b) => UnitDecomposition (a :/ b) where
    type UnitStructure (a :/ b) = UnitStructure a  ++ NegatePowers (UnitStructure b)

instance (Unit a, KnownRatio p) => UnitDecomposition (a :^ p) -- TODO
--    where type UnitStructure (a :^ p) = UnitStructure a ++

-----------------------------------------------------------------------------






