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


module SGC.Object.Internal.TypeMap (

  TypeMap(TMap, TMContains, tmContains, tmFind, tmGet)
, UserTypeMapKey(..)
, tmEmpty

, TypeMapChange(TMUpdateResult, TMUpdateFunc, tmUpdate)
, TMSetValue(..), TMChangeValue(..)

, TypeMapContext(..), ContextMapKey, ctxMapKey

, TypeSubMap(..)

-- * Internal

, TMChange(..), TypeMapChangeT(..), TMKeyType(..), TypeMapKeyType(..)
, TMValue
, proxyHead, proxyTail


) where

import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality
import Data.Maybe (isJust)

import TypeNum.TypeFunctions (ContainsEach)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Maps 'TypeMapKey' types in `tMap` parameter to corresponding 'TMValue's.
class TypeMap (tMap :: [*]) where
  data TMap tMap :: *

  type TMContains tMap tk :: Bool
  tmContains :: (TypeMapKey tk) => TMap tMap -> tk -> Bool

  tmFind :: (TypeMapKey tk) => TMap tMap -> tk -> Maybe (TMValue tk)
  tmGet  :: (TypeMapKey tk) => TMap tMap -> tk -> TMValue tk

  tmFindP :: (TypeMapKey tk) => TMap tMap -> Proxy (tk :: *) -> Maybe (tk, TMValue tk)

class (Typeable tk) => TypeMapKey tk where type TMValue tk :: *

-----------------------------------------------------------------------------

tmEmpty :: TMap '[]
tmEmpty = TMNil

instance TypeMap '[] where
  data TMap '[] = TMNil

  type TMContains '[] tk = False
  tmContains TMNil _ = False

  tmGet   TMNil _ = error "`tmGet` from `TMNil`"
  tmFind  TMNil _ = Nothing
  tmFindP TMNil _ = Nothing


instance (Typeable k, Eq k, TypeMap ks) =>
  TypeMap (k ': ks) where
    data TMap (k ': ks) = TMCons k (TMValue k) (TMap ks)

    type TMContains (k ': ks) tk = k == tk || TMContains ks tk

    tmContains (TMCons k v ks) tk = sameKey' k tk || tmContains ks tk

    tmFind (TMCons k v ks) tk =
      case k `sameKey` tk of Just Refl -> Just v
                             _         -> tmFind ks tk
    tmGet tm k = case tmFind tm k of Just v -> v
                                     _      -> error "Key not found in TMap"

    tmFindP (TMCons k v ks) k' =
      case  sameKeyP k' k of Just Refl -> Just (k, v)
                             _         -> tmFindP ks k'


sameKey :: (Typeable k1, Typeable k2) => k1 -> k2 -> Maybe (k1 :~: k2)
sameKey _ _ = eqT

sameKey' :: (Typeable k1, Typeable k2, Eq k1) => k1 -> k2 -> Bool
sameKey' = (isJust .) . sameKey

sameKeyP :: (Typeable k1, Typeable k2) => Proxy k1 -> k2 -> Maybe (k1 :~: k2)
sameKeyP _ _ = eqT

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
  where TMKeyType (ContextMapKey ctx k) = TypeMapKeyInternal
        TMKeyType k                     = TypeMapKeyUser


instance (Typeable k, UniversalTypeMapKey k (TMKeyType k)) => TypeMapKey k where
  type TMValue k = UTMValue k (TMKeyType k)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (TypeMap tMap, Typeable ctx) =>
  TypeMapContext ctx (tMap :: [*]) where
    type CtxKeyValue ctx :: * -> *
    tmCtxFind :: (UserTypeMapKey tk) => ctx -> TMap tMap -> tk -> Maybe (CtxKeyValue ctx (UserKeyValue tk))
    tmCtxGet  :: (UserTypeMapKey tk) => ctx -> TMap tMap -> tk ->        CtxKeyValue ctx (UserKeyValue tk)

    tmCtxFind ctx m = tmFind m . ctxMapKey ctx
    tmCtxGet  ctx m = tmGet m  . ctxMapKey ctx

data ContextMapKey (ctx :: *) (k :: *) = ContextMapKey
  deriving (Typeable, Eq)
ctxMapKey :: ctx -> k -> ContextMapKey ctx k
ctxMapKey _ _ = ContextMapKey


instance (Typeable ctx, Typeable k, UserTypeMapKey k) =>
  InternalTypeMapKey (ContextMapKey ctx k) where
    type IKeyValue (ContextMapKey ctx k) = CtxKeyValue ctx (UserKeyValue k)

-----------------------------------------------------------------------------



-- class TypeSubMap (sub :: [*]) (mp :: [*]) where
--   subTMap :: Proxy sub -> TMap mp -> Maybe (TMap sub)
--
-- instance TypeSubMap sub '[] where subTMap _ _ = Nothing
-- instance TypeSubMap '[] mp  where subTMap _ _ = Just tmEmpty
-- instance ( TypeMap mp, Typeable h, TypeSubMap t mp
--          , UniversalTypeMapKey h (TMKeyType h)
--           ) =>
--   TypeSubMap (h ': t) mp where
--     subTMap sub mp = case tmFindP mp (proxyHead sub)
--                       of Just (k, v) -> TMCons k v <$> subTMap (proxyTail sub) mp

proxyHead :: Proxy (h ': t) -> Proxy h
proxyHead = const Proxy

proxyTail :: Proxy (h ': t) -> Proxy t
proxyTail = const Proxy

-- class TypeSubMap (mp :: [*]) (sub :: [*]) where
--   subTMap :: Proxy sub -> TMap mp -> TMap sub

-- instance (ContainsEach mp sub ~ True) => TypeSubMap mp sub where
--   subTMap _ tmap = undefined


-- subTMap' :: Proxy (sub :: [*]) -> TMap (mp :: [*]) -> Maybe (TMap sub)
-- subTMap' sub mp = case tmFindP mp (proxyHead sub)
--                   of Just (k, v) -> TMCons k v <$> subTMap' (proxyTail sub) mp

class TypeSubMap (sub :: [*]) where
  subTMap :: (TypeMap mp) => Proxy sub -> TMap (mp :: [*]) -> Maybe (TMap sub)

instance TypeSubMap '[] where subTMap _ _ = Just tmEmpty
instance (Typeable h, UniversalTypeMapKey h (TMKeyType h), TypeSubMap t) =>
  TypeSubMap (h ': t) where
    subTMap sub mp =
      case tmFindP mp (proxyHead sub)
        of Just (k, v) -> TMCons k v <$> subTMap (proxyTail sub) mp
           _           -> Nothing

-----------------------------------------------------------------------------
