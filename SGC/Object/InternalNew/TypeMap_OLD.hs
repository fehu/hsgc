-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Internal.TypeMap
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module SGC.Object.InternalNew.TypeMap where

import SGC.Object.InternalNew.TypeSet
import TypeNum.TypeFunctions

import Data.Kind (type (*), Constraint)
import Data.Typeable
import Data.Type.Bool
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data UniversalKeyType = UniversalKeyAsIs
                      | UniversalKeyWrapped (* -> *)

class (Typeable k) => UniversalTypeKey k (kt :: UniversalKeyType) where
  type UTypeValue k kt :: *


class (Typeable k) => TypeKey k where type KeyValue k :: *
data WrappedTypeKey (wrap :: * -> *) k = WrappedTypeKey k
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

type family TypeKeyType k :: UniversalKeyType
  where TypeKeyType (WrappedTypeKey w k) = UniversalKeyWrapped w
        TypeKeyType k                    = UniversalKeyAsIs

type TypeKey' k = UniversalTypeKey k (TypeKeyType k)
type TypeVal' k = UTypeValue k (TypeKeyType k)


instance (TypeKey k, Typeable wrap) =>
  UniversalTypeKey (WrappedTypeKey wrap k) (UniversalKeyWrapped wrap) where
    type UTypeValue (WrappedTypeKey wrap k) (UniversalKeyWrapped wrap) = wrap (KeyValue k)

instance (TypeKey k) =>
  UniversalTypeKey k UniversalKeyAsIs where
    type UTypeValue k UniversalKeyAsIs = KeyValue k


instance TypesEq (WrappedTypeKey w k1) (WrappedTypeKey w k2) where
  type (WrappedTypeKey w k1) ~=~ (WrappedTypeKey w k2) = k1 ~=~ k2

instance TypesOrd (WrappedTypeKey w k1) (WrappedTypeKey w k2) where
  type Cmp (WrappedTypeKey w k1) (WrappedTypeKey w k2) = Cmp k1 k2


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMap (keys :: TypeSet) where
  data TMap keys :: *

  tmKeys   :: TMap keys -> TSet keys
  tmFind   :: (TypeKey' k) => k -> TMap keys -> Maybe (TypeVal' k)
  tmChange :: (TypeKey' k) => k -> (TypeVal' k -> TypeVal' k)
                                -> TMap keys -> Maybe (TMap keys)

instance TypeMap SetNil where
  data TMap SetNil = TMNil
  tmKeys _ = tSet
  tmFind _ _ = Nothing
  tmChange _ _ _ = Nothing

instance ( Typeable h, TypeMap t, TypeSet' t ) =>
  TypeMap (SetCons h t) where
    data TMap (SetCons k t) = TMCons k (TypeVal' k) (TMap t)
    tmKeys _ = tSet
    tmFind k' (TMCons k v t) = case sameT k k'
                                of Just Refl -> Just v
                                   _         -> tmFind k' t
    tmChange k' f (TMCons k v t) = case sameT k k'
                                of Just Refl -> Just $ TMCons k (f v) t
                                   _         -> TMCons k v <$> tmChange k' f t

-----------------------------------------------------------------------------

data KeyVal k = KeyVal k (TypeVal' k)

instance TypeCons TMap (SetCons h t) where
  type THead TMap (SetCons h t) = KeyVal h
  type TTail TMap (SetCons h t) = t
  tCons   (KeyVal k v) t = TMCons k v t
  tUncons (TMCons k v t) = (KeyVal k v, t)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tmEmpty :: TMap SetNil
tmEmpty = TMNil

class TypeMapPut (keys :: TypeSet) k where
  tmPut'  :: k -> TypeVal' k -> TMap keys -> TMap (InsertOrdering k keys)

tmPut :: TypeMapPut keys k =>
          TMap keys -> (k, TypeVal' k) -> TMap (InsertOrdering k keys)

-----------------------------------------------------------------------------

tmPut mp (k, v) = tmPut' k v mp

instance TypeMapPut SetNil k where
  tmPut' k' v' _ = TMCons k' v' TMNil

instance ( InsertOrderingNE' (SetCons h t) k (Cmp h k)
         , Typeable h, Typeable k
          ) =>
  TypeMapPut (SetCons h t) k where
    tmPut' k' v' t = insertOrdering tUncons tCons k' (KeyVal k' v') t


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapPrintKeys keys where tmPrintKeys' :: TMap keys -> [String]

tmPrintKeys :: (TypeMapPrintKeys keys) => TMap keys -> String
tmPrintKeys = strSqPar . concat . intersperse ","  . tmPrintKeys'


class TypeMapPrint keys where tmPrint' :: TMap keys -> [String]

tmPrint :: TypeMapPrint keys => TMap keys -> String
tmPrint = strSqPar . concat . intersperse ", " . tmPrint'

-----------------------------------------------------------------------------

instance (Show h, TypeMapPrintKeys t) => TypeMapPrintKeys (SetCons h t) where
  tmPrintKeys' (TMCons k _ t) = show k : tmPrintKeys' t
instance TypeMapPrintKeys SetNil where
  tmPrintKeys' _ = []


instance (Show h, Show (TypeVal' h), TypeMapPrint t) =>
  TypeMapPrint (SetCons h t) where
    tmPrint' (TMCons k v t) = (show k ++ " -> " ++ show v) : tmPrint' t

instance TypeMapPrint SetNil where
    tmPrint' _ = []

strSqPar x = "[" ++ x ++ "]"

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapSelect (sub :: TypeSet) (keys :: TypeSet) where
  tmSelect :: TSet sub -> TMap keys -> Maybe (TMap sub)

-----------------------------------------------------------------------------

instance TypeMapSelect SetNil set             where tmSelect _ _ = Just tmEmpty
instance TypeMapSelect (SetCons h' t') SetNil where tmSelect _ _ = Nothing

instance ( TypeSet' t', Typeable h', Typeable h, TypeMapSelect t' t
         , TypeMapSelect (SetCons h' t') t
          ) =>
  TypeMapSelect (SetCons h' t') (SetCons h t) where
    tmSelect sub (TMCons h v t) =
      let (h', t') = tUncons sub
      in case sameTP1 h' h
          of Just Refl -> TMCons h v <$> tmSelect t' t
             _         -> tmSelect sub t

-----------------------------------------------------------------------------

sameT :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT

sameTP1 :: (Typeable a, Typeable b) => Proxy a -> b -> Maybe (a :~: b)
sameTP1 _ _ = eqT

-----------------------------------------------------------------------------

class (Contains k keys ~ True) =>
  TypeMapContains k (keys :: TypeSet) where
    tmGet :: k -> TMap keys -> TypeVal' k

instance (TypeMap keys, TypeKey' k, Contains k keys ~ True) =>
  TypeMapContains k keys where
    tmGet = (fromJust .) . tmFind

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Test
-----------------------------------------------------------------------------


data A = A deriving (Typeable, Show)
instance TypeKey A where type KeyValue A = String

ioKey :: k ->  WrappedTypeKey IO k
ioKey = WrappedTypeKey

testPut = tmEmpty `tmPut` (ioKey A, return "Test")


data B = B deriving (Typeable, Show)
instance TypeKey B where type KeyValue B = Int

instance TypesEq A B  where type A ~=~ B = False
instance TypesEq B A  where type B ~=~ A = False
instance TypesOrd A B where type Cmp A B = LT
instance TypesOrd B A where type Cmp B A = GT

testTM = tmPut' A "A"
       $ tmPut' B  5 tmEmpty

testSubs :: TSet (PutInSet B SetNil)
testSubs = tSet

testSelect :: Maybe (TMap (PutInSet B SetNil))
testSelect = tmSelect testSubs testTM


testTM' = testPut `tmPut` (ioKey B, return 5)

type TestSubs' = PutInSet (WrappedTypeKey IO B) SetNil
testSubs' :: TSet TestSubs'
testSubs' = tSet

testSelect' :: Maybe (TMap TestSubs')
testSelect' = tmSelect testSubs' testTM'

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapChange (upd :: * -> *) k (keys :: TypeSet) where
  type TMChangeResult upd k keys :: TypeSet

  tmUpdate :: (TypeKey k) => k
                          -> upd (TypeVal' k)
                          -> TMap keys
                          -> TMap (TMChangeResult upd k keys)

-- | Only new ones
newtype TMPutValue v = TMPutValue v
-- | Only existing ones
newtype TMUpdValue v = TMUpdValue (v -> v)

-----------------------------------------------------------------------------

-- | Put new value
instance (Contains k keys ~ False, TypeMapPut keys k) =>
  TypeMapChange TMPutValue k keys where
    type TMChangeResult TMPutValue k keys = InsertOrdering k keys
    tmUpdate k (TMPutValue v) = tmPut' k v

-- | Change existing value
instance (Contains k keys ~ True, TypeMap keys, TypeKey' k) =>
  TypeMapChange TMUpdValue k keys where
    type TMChangeResult TMUpdValue k keys = keys
    tmUpdate k (TMUpdValue f) = fromJust . tmChange k f


-----------------------------------------------------------------------------
