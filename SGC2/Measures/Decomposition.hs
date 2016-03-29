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


{-# LANGUAGE ExistentialQuantification
           , GADTs
           , TypeOperators
           , UndecidableInstances
           , Rank2Types
--           , PolyKinds
       #-}

module SGC2.Measures.Decomposition where

import SGC2.Measures.Definitions

import Data.Typeable
import Data.Type.Equality
import Data.Type.Bool

import TypeNum.Integer
import TypeNum.Rational

-----------------------------------------------------------------------------

--newtype (Unit u) => AtomWeight u (i :: TInt) = AtomWeight u

newtype (Unit u) => AtomWeight u (i :: TInt) = AtomWeight u

data P a b = P a b

atomUnit (AtomWeight u) = u

atomWeight' :: AtomWeight a i -> Int' i
atomWeight' _ = Int'

atomWeight :: (TIntValue i) => AtomWeight a i -> Integer
atomWeight = intValue . atomWeight'


-----------------------------------------------------------------------------

class (Unit u) => UnitDecomposition u where type UnitStructure' u :: [(*, TInt)]


-----------------------------------------------------------------------------

---- map types list
--type family (<$>) (f :: * -> *) (l :: [*]) :: [*] where
--    f <$> '[]      = '[]
--    f <$> (h ': t) = (f h) ': (f <$> t)


--type family (++) (l1 :: [*]) (l2 :: [*]) :: [*] where
--    '[]      ++ l = l
--    (h ': t) ++ l = h ': (t ++ l)


---- map types list
--type family (<$>) (f :: * -> (*, TInt)) (l :: [*]) :: [(*, TInt)] where
--    f <$> '[]      = '[]
--    f <$> (h ': t) = (f h) ': (f <$> t)


type family (++) (l1 :: [(*, TInt)]) (l2 :: [(*, TInt)]) :: [(*, TInt)] where
    '[]      ++ l = l
    (h ': t) ++ l = h ': (t ++ l)


type family NegatePowers (l :: [(*, TInt)]) :: [(*, TInt)] where
    NegatePowers '[] = '[]
    NegatePowers ('(u, x) ': t) = '(u, Negate x) ': NegatePowers t

-----------------------------------------------------------------------------

type family UnitWeight (l :: [(*, TInt)]) (u :: *) :: TInt where
    UnitWeight '[]             u = Zero
    UnitWeight ('(u', i) ': t) u = If (u == u') i (UnitWeight t u)


type family Contains (k :: *) (l :: [*]) :: Bool where
    Contains k '[]            = False
    Contains k (k' ': t) = k == k' || Contains k t


type family ContainsKey (k :: *) (l ::[(*, TInt)]) :: Bool where
    ContainsKey k '[]            = False
    ContainsKey k ('(u, i) ': t) = k == u || ContainsKey k t



type family UniqueKeys' (l   :: [(*, TInt)]) (acc :: [*]) :: [*] where
    UniqueKeys' '[] acc            = acc
    UniqueKeys' ('(u, i) ': t) acc = UniqueKeys' t (If (u `Contains` acc) acc (u ': acc))

type UniqueKeys (l :: [(*, TInt)]) = UniqueKeys' l '[]

-----------------------------------------------------------------------------

type family DecompositionEq (a :: [(*, TInt)]) (b :: [(*, TInt)]) :: Bool where
    DecompositionEq '[] '[] = True
    DecompositionEq ('(ua, ia) ': ta) ('(ub, ib) ': tb) = ua == ub &&
                                                          ia == ib &&
                                                          DecompositionEq ta tb




type family DecompositionSum (as :: [(*, TInt)]) (bs :: [(*, TInt)]) :: [(*, TInt)] where
    DecompositionSum as bs = DecompositionSum' (UniqueKeys (as ++ bs)) as bs


type family DecompositionSum' ks as bs where
    DecompositionSum' '[] as bs      = '[]
    DecompositionSum' (k ': t) as bs = '(k, UnitWeight as k + UnitWeight bs k)
                                    ': DecompositionSum' t as bs



-----------------------------------------------------------------------------


type (!==) a b = DecompositionEq (UnitStructure' a) (UnitStructure' b) ~ True


--type family (!*) a b where


--type family (===) a b where
--    a === b = DecompositionEq (UnitStructure' a) (UnitStructure' b)

--class UnitEq a b where a === b


-----------------------------------------------------------------------------

data AtomsWeights (l :: [(*, TInt)]) where
    WeightsNil  :: AtomsWeights '[]
    WeightsCons :: AtomWeight u i -> AtomsWeights l -> AtomsWeights ('(u,i) ': l)

type UnitStructure u = AtomsWeights (UnitStructure' u)

-----------------------------------------------------------------------------

instance (Unit a, Unit b) => UnitDecomposition (a * b) where
    type UnitStructure' (a * b) = UnitStructure' a  ++ UnitStructure' b
instance (Unit a, Unit b) => UnitDecomposition (a / b) where
    type UnitStructure' (a / b) = UnitStructure' a  ++ NegatePowers (UnitStructure' b)
--instance (Unit a, AsRational p,  IntValuesForRational (AsRational' p)) => UnitDecomposition (a ^ p) where

-----------------------------------------------------------------------------






