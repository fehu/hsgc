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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SGC.Object.InternalNew.TypeMap where

import SGC.Object.InternalNew.TypeSet

import TypeNum.TypeFunctions

import Data.Typeable
import Data.Maybe (fromJust)
import Data.List (intercalate)

import qualified Data.Functor.Identity as Id

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (Typeable k) => TypeKey k where type KeyValue k :: *

class TypeMap (wrap :: * -> *) (keys :: TypeSet) where
  data TMap wrap keys :: *

  tmKeys   :: TMap wrap keys -> TSet keys
  tmFind   :: (TypeKey k) => k -> TMap wrap keys -> Maybe (wrap (KeyValue k))
  tmChange :: (TypeKey k) => k -> (wrap (KeyValue k) -> wrap (KeyValue k))
                               -> TMap wrap keys -> Maybe (TMap wrap keys)


type TM wrap = TMap wrap SetNil

tmEmpty :: TM wrap
tmEmpty = TMNil

-----------------------------------------------------------------------------

instance TypeMap wrap SetNil where
  data TMap wrap SetNil = TMNil
  tmKeys _ = tSet
  tmFind _ _ = Nothing
  tmChange _ _ _ = Nothing

instance ( Typeable h, TypeMap wrap t, TypeSet' t ) =>
  TypeMap wrap (SetCons h t) where
    data TMap wrap (SetCons k t) = TMCons k (wrap (KeyValue k)) (TMap wrap t)
    tmKeys _ = tSet
    tmFind k' (TMCons k v t) = case sameT k k'
                                of Just Refl -> Just v
                                   _         -> tmFind k' t
    tmChange k' f (TMCons k v t) = case sameT k k'
                                of Just Refl -> Just $ TMCons k (f v) t
                                   _         -> TMCons k v <$> tmChange k' f t

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapUpdate (upd :: * -> *) k (keys :: TypeSet) where
  type TMUpdateResult upd k keys :: TypeSet

  tmUpdate :: (TypeKey k, TypeMap wrap keys) =>
              k
           -> upd (wrap (KeyValue k))
           -> TMap wrap keys
           -> TMap wrap (TMUpdateResult upd k keys)

-- | Put new value in TypeMap
newtype TMValuePut v = TMValuePut v
-- | Change existing value in TypeMap
newtype TMValueUpd v = TMValueUpd (v -> v)
-- | Replace existing value in TypeMap
newtype TMValueReplace v = TMValueReplace v

-----------------------------------------------------------------------------

-- | Put new value
instance (Contains k keys ~ False, TypeMapPut keys k) =>
  TypeMapUpdate TMValuePut k keys where
    type TMUpdateResult TMValuePut k keys = InsertOrdering k keys
    tmUpdate k (TMValuePut v) = tmPutW' k v

-- | Change existing value
instance (Contains k keys ~ True) =>
  TypeMapUpdate TMValueUpd k keys where
    type TMUpdateResult TMValueUpd k keys = keys
    tmUpdate k (TMValueUpd f) = fromJust . tmChange k f

-- | Replace existing value
instance (Contains k keys ~ True) =>
  TypeMapUpdate TMValueReplace k keys where
    type TMUpdateResult TMValueReplace k keys = keys
    tmUpdate k (TMValueReplace v) = fromJust . tmChange k (const v)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data KeyVal wrap k = KeyVal k (wrap (KeyValue k))

instance TypeCons (TMap wrap) (SetCons h t) where
  type THead (TMap wrap) (SetCons h t) = KeyVal wrap h
  type TTail (TMap wrap) (SetCons h t) = t
  tCons   (KeyVal k v)   = TMCons k v
  tUncons (TMCons k v t) = (KeyVal k v, t)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class Wrap (wrap :: * -> *) where doWrap :: a -> wrap a

class TypeMapPut (keys :: TypeSet) k where
  tmPutW'  :: k -> wrap (KeyValue k) -> TMap wrap keys
                -> TMap wrap (InsertOrdering k keys)

tmPutW :: TypeMapPut keys k =>
          TMap wrap keys -> (k, wrap (KeyValue k))
                         -> TMap wrap (InsertOrdering k keys)

tmPut' :: (TypeMapPut keys k, Wrap wrap) =>
           k -> KeyValue k -> TMap wrap keys
        -> TMap wrap (InsertOrdering k keys)

tmPut :: (TypeMapPut keys k, Wrap wrap) =>
           TMap wrap keys -> (k, KeyValue k)
                          -> TMap wrap (InsertOrdering k keys)


class (Contains k keys ~ True) =>
  TypeMapContains k (keys :: TypeSet) where
    tmGet :: (TypeMap wrap keys) => k -> TMap wrap keys -> wrap (KeyValue k)


-----------------------------------------------------------------------------

tmPutW mp (k, v) = tmPutW' k v mp
tmPut' k = tmPutW' k . doWrap
tmPut mp (k, v) = tmPutW' k (doWrap v) mp

instance TypeMapPut SetNil k where
  tmPutW' k' v' _ = TMCons k' v' TMNil

instance ( InsertOrderingNE' (SetCons h t) k (Cmp h k)
         , Typeable h, Typeable k ) =>
  TypeMapPut (SetCons h t) k where
    tmPutW' k' v' = insertOrdering tUncons tCons k' (KeyVal k' v')

instance (Contains k keys ~ True, TypeKey k) =>
  TypeMapContains k keys where
    tmGet = (fromJust .) . tmFind


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapSelect (sub :: TypeSet) (keys :: TypeSet) where
  tmSelect :: TSet sub -> TMap wrap keys -> Maybe (TMap wrap sub)

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
-----------------------------------------------------------------------------

class TypeMapPrintKeys keys where tmPrintKeys' :: TMap wrap keys -> [String]
class TypeMapPrint wrap keys where tmPrint' :: TMap wrap keys -> [String]

tmPrintKeys :: (TypeMapPrintKeys keys)  => TMap wrap keys -> String
tmPrint     :: (TypeMapPrint wrap keys) => TMap wrap keys -> String

-----------------------------------------------------------------------------

tmPrintKeys = strSqPar . intercalate ","  . tmPrintKeys'
tmPrint = strSqPar . intercalate ", " . tmPrint'


instance (Show h, TypeMapPrintKeys t) => TypeMapPrintKeys (SetCons h t) where
  tmPrintKeys' (TMCons k _ t) = show k : tmPrintKeys' t
instance TypeMapPrintKeys SetNil where
  tmPrintKeys' _ = []


instance (Show h, Show (wrap (KeyValue h)), TypeMapPrint wrap t) =>
  TypeMapPrint wrap (SetCons h t) where
    tmPrint' (TMCons k v t) = (show k ++ " -> " ++ show v) : tmPrint' t

instance TypeMapPrint wrap SetNil where tmPrint' _ = []

strSqPar x = "[" ++ x ++ "]"

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

sameT :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT

sameTP1 :: (Typeable a, Typeable b) => Proxy a -> b -> Maybe (a :~: b)
sameTP1 _ _ = eqT

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Test
-----------------------------------------------------------------------------


data A = A deriving (Typeable, Show)
instance TypeKey A where type KeyValue A = String

data B = B deriving (Typeable, Show)
instance TypeKey B where type KeyValue B = Int

instance TypesEq A B  where type A ~=~ B = False
instance TypesEq B A  where type B ~=~ A = False
instance TypesOrd A B where type Cmp A B = LT
instance TypesOrd B A where type Cmp B A = GT

instance Wrap Id.Identity where doWrap = pure

testPut1 = (tmEmpty :: TM IO)          `tmPutW` (A, return "Test")
testPut2 = (tmEmpty :: TM Id.Identity) `tmPut`  (A, "Test")

testTM = tmPutW' A (return "A")
       $ tmPut'  B 5
      (tmEmpty :: TM Id.Identity)

testSubs :: TSet (PutInSet B SetNil)
testSubs = tSet

testSelect :: Maybe (TMap Id.Identity (PutInSet B SetNil))
testSelect = tmSelect testSubs testTM


-----------------------------------------------------------------------------
