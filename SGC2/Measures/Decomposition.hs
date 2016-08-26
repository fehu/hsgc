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
           , FlexibleInstances
       #-}

module SGC2.Measures.Decomposition where

import SGC2.Measures.Definitions

import Data.Type.Equality
import Data.Type.Bool

import TypeNum.Rational


-----------------------------------------------------------------------------

class (Unit u) => UnitDecomposition u where type UnitStructure u :: [(*, TRational)]
                                            type UnitHash      u :: Nat


-----------------------------------------------------------------------------

type family (++) (l1 :: [(*, TRational)]) (l2 :: [(*, TRational)]) :: [(*, TRational)] where
    '[]      ++ l = l
    (h ': t) ++ l = h ': (t ++ l)


type family NegatePowers (l :: [(*, TRational)]) :: [(*, TRational)] where
    NegatePowers '[] = '[]
    NegatePowers ('(u, x) ': t) = '(u, Negate x) ': NegatePowers t


type family MultPowers (l :: [(*, TRational)]) (p :: TRational) :: [(*, TRational)] where
    MultPowers '[] p = '[]
    MultPowers ('(u, x) ': t) p = '(u, x*p) ': MultPowers t p

-----------------------------------------------------------------------------


type family DecompositionEq (a :: [(*, TRational)]) (b :: [(*, TRational)]) :: Bool where
    DecompositionEq '[] '[] = True
    DecompositionEq ('(ua, ia) ': ta) ('(ub, ib) ': tb) = ua == ub &&
                                                          ia == ib &&
                                                          DecompositionEq ta tb

-----------------------------------------------------------------------------

type family Compare a b :: Ordering where
    Compare a b = Cmp (UnitHash a) (UnitHash b)


type family FilterOrd' (ord :: Ordering) (e :: k) (l :: [(k, TRational)]) :: [(k, TRational)] where
    FilterOrd' ord e '[] = '[]
    FilterOrd' ord e ('(u, n) ': t) = If (Compare e u == ord ) ('(u, n) ': FilterOrd' ord e t)
                                                               (FilterOrd' ord e t)


type family TQSort' (l :: [(*, TRational)]) :: [(*, TRational)] where
    TQSort' '[] = '[]
    TQSort' ('(u, n) ': t) = TQSort' (FilterOrd' LT u t)
                          ++ '[ '(u, n) ]
                          ++ TQSort' (FilterOrd' GT u t)

type family DecompositionEq' (a :: [(*, TRational)]) (b :: [(*, TRational)]) :: Bool where
    DecompositionEq' '[] '[] = True
    DecompositionEq' as bs   = DecompositionEq (TQSort' as) (TQSort' bs)

-----------------------------------------------------------------------------

type (!==) a b = DecompositionEq' (UnitStructure a) (UnitStructure b) ~ True

-----------------------------------------------------------------------------

instance (Unit a, Unit b) => UnitDecomposition (a :* b) where
    type UnitStructure (a :* b) = UnitStructure a  ++ UnitStructure b

instance (Unit a, Unit b) => UnitDecomposition (a :/ b) where
    type UnitStructure (a :/ b) = UnitStructure a  ++ NegatePowers (UnitStructure b)

instance (Unit a, MayRational p, KnownRatio (AsRational p)) => UnitDecomposition (Pow a p)
    where type UnitStructure (Pow a p) = MultPowers (UnitStructure a) p

-----------------------------------------------------------------------------






