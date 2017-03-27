-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Generic.TypeMap
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | TypeMap maps type keys `tk` to some assosiated values `tc tk`.
--
-- TypeMap must provide type functions:
--   1. Contains :: TypeMap -> tk -> Bool
--   2. Add      :: TypeMap -> tk -> TypeMap -- needs default `tc tk`
--   3. Remove   :: TypeMap -> tk -> TypeMap
--
-- TypeMap must provide functions:
--   1. get :: TypeMap -> tk -> tc tk
--   2. set :: TypeMap -> tk -> tc tk -> TypeMap
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module SGC.Object.Generic.TypeMap (

  TypeMap(TMap, TMContains, tmFind, tmGet)
, UserTypeMapKey(..)
, tmEmpty

, TypeMapChange(TMUpdateResult, TMUpdateFunc, tmUpdate)
, TMSetValue(..), TMChangeValue(..)

, TypeMapContext(..), ContextMapKey, ctxMapKey

-- * Internal

, TMChange(..), TypeMapChangeT(..), TMKeyType(..), TypeMapKeyType(..)

) where

import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Maps 'TypeMapKey' types in `tMap` parameter to corresponding 'TMValue's.
class TypeMap (tMap :: [*]) where
  data TMap tMap :: *

  type TMContains tMap tk :: Bool

  tmFind :: (TypeMapKey tk) => TMap tMap -> tk -> Maybe (TMValue tk)
  tmGet  :: (TypeMapKey tk) => TMap tMap -> tk -> TMValue tk

class (Typeable tk) => TypeMapKey tk where type TMValue tk :: *

-----------------------------------------------------------------------------

tmEmpty :: TMap '[]
tmEmpty = TMNil

instance TypeMap '[] where
  data TMap '[] = TMNil

  type TMContains '[] tk = False

  tmGet  TMNil _ = error "`tmGet` from `TMNil`"
  tmFind TMNil _ = Nothing


instance (Typeable k, Eq k, TypeMap ks) =>
  TypeMap (k ': ks) where
    data TMap (k ': ks) = TMCons k (TMValue k) (TMap ks)

    type TMContains (k ': ks) tk = k == tk || TMContains ks tk

    tmFind (TMCons k v ks) tk =
      case k `sameKey` tk of Just Refl -> Just v
                             _         -> tmFind ks tk
    tmGet tm k = case tmFind tm k of Just v -> v
                                     _      -> error "Key not found in TMap"


sameKey :: (Typeable k1, Typeable k2, Eq k1) => k1 -> k2 -> Maybe (k1 :~: k2)
sameKey _ _ = eqT


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMapChange (tMap :: [*]) tk where
  type TMUpdateResult tMap tk :: [*]
  type TMUpdateFunc   tMap tk :: * -> *

  tmUpdate :: (TypeMapKey tk) => TMap tMap
                              -> tk
                              -> TMUpdateFunc tMap tk (TMValue tk)
                              -> TMap (TMUpdateResult tMap tk)

newtype TMSetValue v    = TMSetValue v
newtype TMChangeValue v = TMChangeValue (v -> v)

-----------------------------------------------------------------------------

data TypeMapChangeT = TypeMapUpdate | TypeMapGrowth

class TypeMapChange' (tMap :: [*]) tk (change :: TypeMapChangeT) where
  type TMUpdateResult' change tMap tk :: [*]
  type TMUpdateFunc'   change tMap tk :: * -> *

  tmUpdate' :: (TypeMapKey tk) => Proxy change
                              -> TMap tMap
                              -> tk
                              -> TMUpdateFunc' change tMap tk (TMValue tk)
                              -> TMap (TMUpdateResult' change tMap tk)


type family TMChange (m :: [*]) (k :: *) :: TypeMapChangeT
  where
    TMChange '[] k = TypeMapGrowth
    TMChange  m  k = If (TMContains m k) TypeMapUpdate TypeMapGrowth

tmChange :: TMap m -> k -> Proxy (TMChange m k)
tmChange _ _ = Proxy

instance (TypeMapChange' tMap tk (TMChange tMap tk)) =>
  TypeMapChange tMap tk where
    type TMUpdateResult tMap tk = TMUpdateResult' (TMChange tMap tk) tMap tk
    type TMUpdateFunc   tMap tk = TMUpdateFunc'   (TMChange tMap tk) tMap tk
    tmUpdate m k = tmUpdate' (tmChange m k) m k

-----------------------------------------------------------------------------

instance ( Typeable k', Eq tk
         , TypeMapChange' ks tk TypeMapUpdate
         , TMUpdateResult' TypeMapUpdate ks tk ~ ks
         , TMUpdateFunc' 'TypeMapUpdate ks tk ~ TMChangeValue
          ) =>
  TypeMapChange' (k' ': ks) tk TypeMapUpdate where
    type TMUpdateResult' TypeMapUpdate (k' ': ks) k = k' ': ks
    type TMUpdateFunc'   TypeMapUpdate (k' ': ks) k = TMChangeValue
    tmUpdate' t (TMCons k' v ks) k f'@(TMChangeValue f) =
      case k `sameKey` k'
        of Just Refl -> TMCons k (f v) ks
           _         -> TMCons k' v $ tmUpdate' t ks k f'

instance TypeMapChange' m tk TypeMapGrowth where
  type TMUpdateResult' TypeMapGrowth m k = k ': m
  type TMUpdateFunc'   TypeMapGrowth m k = TMSetValue
  tmUpdate' _ m k f'@(TMSetValue v) = TMCons k v m




-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class UniversalTypeMapKey k (kt :: TypeMapKeyType) where
  type UTMValue k kt :: *

data TypeMapKeyType = TypeMapKeyInternal | TypeMapKeyUser

class                 InternalTypeMapKey k where type IKeyValue k :: *
class (Typeable k) => UserTypeMapKey     k where type UserKeyValue k :: *

-----------------------------------------------------------------------------


instance (InternalTypeMapKey k) => UniversalTypeMapKey k TypeMapKeyInternal where
  type UTMValue k TypeMapKeyInternal = IKeyValue k

instance (UserTypeMapKey k) => UniversalTypeMapKey k TypeMapKeyUser where
  type UTMValue k TypeMapKeyUser = UserKeyValue k

-----------------------------------------------------------------------------

type family TMKeyType k :: TypeMapKeyType
  where TMKeyType (ContextMapKey ctx val k) = TypeMapKeyInternal
        TMKeyType k                         = TypeMapKeyUser

instance (Typeable k, UniversalTypeMapKey k (TMKeyType k)) => TypeMapKey k where
  type TMValue k = UTMValue k (TMKeyType k)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (TypeMap tMap) =>
  TypeMapContext ctx (val :: * -> *) (tMap :: [*]) where -- | ctx -> val where
    tmCtxFind :: (UserTypeMapKey tk) => ctx -> TMap tMap -> tk -> Maybe (val (UserKeyValue tk))
    tmCtxGet  :: (UserTypeMapKey tk) => ctx -> TMap tMap -> tk ->        val (UserKeyValue tk)

data ContextMapKey (ctx :: *) (val :: * -> *) (k :: *) = ContextMapKey
  deriving Typeable
ctxMapKey :: ctx -> k -> ContextMapKey ctx val k
ctxMapKey _ _ = ContextMapKey


instance (Typeable ctx, Typeable val, Typeable k, UserTypeMapKey k) =>
  InternalTypeMapKey (ContextMapKey ctx val k) where
    type IKeyValue (ContextMapKey ctx val k) = val (UserKeyValue k)

instance (TypeMap tMap, Typeable ctx, Typeable val) =>
  TypeMapContext ctx val tMap where
    tmCtxFind ctx m = tmFind m . ctxMapKey ctx
    tmCtxGet  ctx m = tmGet m  . ctxMapKey ctx

-----------------------------------------------------------------------------
