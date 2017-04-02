-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Internal.TypeSet
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module SGC.Object.InternalNew.TypeSet where

import Data.Type.Bool
import Data.Typeable
import Data.Maybe (isJust)
import Data.Kind (type (*))

import TypeNum.TypeFunctions

-----------------------------------------------------------------------------


data TypeSet where
  SetNil  :: TypeSet
  SetCons :: * -> TypeSet -> TypeSet

class TypeSet' (ts :: TypeSet) where
  data TSet ts :: *
  tSet :: TSet ts

  tsContains :: (Typeable x) =>                                 x -> TSet ts -> Bool
  tsAll      :: (forall (x :: *) . Typeable x => Proxy x -> Bool) -> TSet ts -> Bool
  tsAny      :: (forall (x :: *) . Typeable x => Proxy x -> Bool) -> TSet ts -> Bool


isSubsetOf   :: (TypeSet' set, TypeSet' sub) => TSet sub -> TSet set -> Bool

-----------------------------------------------------------------------------

type family PutInSet (x :: e) (set :: TypeSet) :: TypeSet
  where PutInSet x set = If (Contains x set) set (InsertOrdering x set)


type IsSetMember  x set = Contains x set
type IsSubsetOf sub set = ContainsEach set sub

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance TContainerElem SetNil (x :: e) where
  type Contains x SetNil = False
  type All   cond SetNil = True
  type Any   cond SetNil = False
  type Prepend  x SetNil = PutInSet x SetNil
  type Append   SetNil x = PutInSet x SetNil
  type Rm       x SetNil = SetNil

instance TContainerElem (SetCons (h :: *) t) (x :: *) where
  type Contains x (SetCons h t) = SearchOrderedKey x (SetCons h t) True False
  type All   cond (SetCons h t) = (cond :$: h) &&  All cond t
  type Any   cond (SetCons h t) = (cond :$: h) ||  Any cond t
  type Prepend  x (SetCons h t) = PutInSet x (SetCons h t)
  type Append   (SetCons h t) x = PutInSet x (SetCons h t)
  type Rm       x (SetCons h t) = If (x == h) t (If (h > x) (SetCons h t)
                                                            (SetCons h (Rm x t)))

-----------------------------------------------------------------------------

instance TypeSet' SetNil where
  data TSet SetNil = TSNil
  tSet = TSNil
  tsContains _ _ = False
  tsAll      _ _ = True
  tsAny      _ _ = False

instance (TypeSet' t, Typeable h) =>
  TypeSet' (SetCons h t) where
    data TSet (SetCons h t) = TSCons (Proxy h) (TSet t)
    tSet = TSCons Proxy tSet
    tsContains x (TSCons h t) = sameProxyT h x || tsContains x t
    tsAll   cond (TSCons h t) = cond h && tsAll cond t
    tsAny   cond (TSCons h t) = cond h || tsAny cond t

isSubsetOf sub set = tsAll (`tsContains` set) sub


-----------------------------------------------------------------------------

type family SearchOrderedKey (x :: *) (set :: TypeSet) keyEq noKey
  where SearchOrderedKey x  SetNil       keyEq noKey = noKey
        SearchOrderedKey x (SetCons h t) keyEq noKey =
          If (x == h) keyEq
            (If (h > x) noKey
              (SearchOrderedKey x t keyEq noKey))

-- type family InsertOrdering x (set :: TypeSet) :: TypeSet
--   where InsertOrdering x SetNil        = SetCons x SetNil
--         InsertOrdering x (SetCons h t) = If (h < x) --  (Cmp h x == LT)
--                                             (SetCons h (InsertOrdering x t))
--                                             (SetCons x (SetCons h t))

sameProxyT x y = isJust $ sameProxyT' x y

sameProxyT' :: (Typeable a, Typeable b) => Proxy a -> b -> Maybe (a :~: b)
sameProxyT' _ _ = eqT

compareTF' :: (Typeable a, Typeable b) => f (SetCons a t) -> b -> Proxy (Cmp a b)
compareTF' _ _ = Proxy


class InsertOrdering' (set :: TypeSet) x where
  type InsertOrdering x set :: TypeSet
  insertOrdering :: (forall (h :: *) (t :: TypeSet) . fs (SetCons h t) -> (f h, fs t) )
                 -> (forall (h :: *) (t :: TypeSet) . f h -> fs t -> fs (SetCons h t) )
                 -> x -> f x
                 -> fs set -> fs (InsertOrdering x set)


instance InsertOrdering' SetNil x where
  type InsertOrdering x SetNil = SetCons x SetNil
  insertOrdering _ cons x fx setf = cons fx setf

-- | for non-empty typeset
class InsertOrderingNE' (set :: TypeSet) x (ord :: Ordering) where
  type InsertOrderingNE x set ord :: TypeSet
  insertOrderingNE :: Proxy ord
                   -> (forall (h :: *) (t :: TypeSet) . fs (SetCons h t) -> (f h, fs t) )
                   -> (forall (h :: *) (t :: TypeSet) . f h -> fs t -> fs (SetCons h t) )
                   -> x -> f x
                   -> fs set -> fs (InsertOrderingNE x set ord)


instance (InsertOrderingNE' (SetCons h t) x (Cmp h x), Typeable h, Typeable x) =>
  InsertOrdering' (SetCons h t) x where
    type InsertOrdering x (SetCons h t) = InsertOrderingNE x (SetCons h t) (Cmp h x)
    insertOrdering uncons cons x fx setf =
      insertOrderingNE (compareTF' setf x) uncons cons x fx setf


instance (InsertOrdering' t x) =>
  InsertOrderingNE' (SetCons h t) x LT where
    type InsertOrderingNE x (SetCons h t) LT = SetCons h (InsertOrdering x t)
    insertOrderingNE _ uncons cons x fx setf =
      let (h, t) = uncons setf
      in cons h (insertOrdering uncons cons x fx t)

instance InsertOrderingNE' (SetCons h t) x GT where
  type InsertOrderingNE x (SetCons h t) GT = (SetCons x (SetCons h t))
  insertOrderingNE _ uncons cons x fx setf = cons fx setf


-----------------------------------------------------------------------------

-- class TypeCons (c :: k -> *) (keys :: k) (h :: *) (t :: k) where
--   tCons   :: h -> c t -> c keys
--   tUncons :: c keys -> (h, c t)
--
--
-- instance (TypeSet' t, Typeable h) =>
--   TypeCons TSet (SetCons h t) (Proxy h) t where
--     tCons _ _ = tSet
--     tUncons _ = (Proxy, tSet)

class TypeCons (c :: k -> *) (keys :: k) where
  type THead c keys :: *
  type TTail c keys :: k
  tCons   :: THead c keys -> c (TTail c keys) -> c keys
  tUncons :: c keys -> (THead c keys, c (TTail c keys))


instance (TypeSet' t, Typeable h) =>
  TypeCons TSet (SetCons h t)  where
    type THead TSet (SetCons h t) = Proxy h
    type TTail TSet (SetCons h t) = t
    tCons _ _ = tSet
    tUncons _ = (Proxy, tSet)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- data SubsetEvidence sub = SubsetEv { withSubSe }




















-----------------------------------------------------------------------------



-- class IsSubset (sub :: TypeSet) (set :: TypeSet) where
  -- type IsSubsetOf sub set :: Bool

-- isSubsetOf :: TSet sub -> TSet set -> Bool
-- isSubsetOf sub SetNil = False

-- isSubsetEv :: TS sub -> TS set -> IsSubsetEvidence sub set
-- isSubsetEv x y = IsSubsetEvidence(\f -> f x y)

-- data IsSubsetEvidence (sub :: TypeSet) (set :: TypeSet) =
--      IsSubsetEvidence {
--       withSubsetEv :: forall r . (IsSubsetOf sub set ~ True) =>
--                       (TS sub -> TS set -> r) -> r
--         }

-- instance IsSubset SetNil set where
--   type IsSubsetOf SetNil set = True
-- instance IsSubset (SetCons h t) SetNil where
--   type IsSubsetOf (SetCons h t) SetNil = False
-- instance IsSubset (SetCons h t) set where
--   type IsSubsetOf (SetCons h t) set = True


-- foo = isSubsetEv (TS :: TS SetNil) (TS :: TS (SetCons "a" SetNil))
-- bar = isSubsetEv (TS :: TS SetNil) (TS :: TS SetNil)
-- baz = isSubsetOf (TS :: TS (SetCons "a" SetNil)) (TS :: TS SetNil)
--
--
-- xx = withSubsetEv foo $ \x y -> "A"

-----------------------------------------------------------------------------
