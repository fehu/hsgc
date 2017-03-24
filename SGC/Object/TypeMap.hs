-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.TypeMap
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

module SGC.Object.TypeMap (

  TypeMap(TMap, TMContains, tmFind, tmGet)
, TypeMapKey(..)
, tmEmpty

, TypeMapChange(TMUpdateResult, TMUpdateFunc, tmUpdate)
, TMSetValue(..), TMChangeValue(..)

, TypeMapContext(..)


) where

import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality

-- import GHC.Exts (Constraint)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypeMap (tMap :: [*]) where -- (keyClass :: * -> Constraint)
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

class (TypeMap tMap) =>
  TypeMapContext ctx (tMap :: [*]) val where
    tmCtxFind :: (TypeMapKey tk, TMValue tk ~ val) => ctx -> TMap tMap -> tk -> Maybe val
    tmCtxGet  :: (TypeMapKey tk, TMValue tk ~ val) => ctx -> TMap tMap -> tk ->       val

instance (TypeMap tMap) =>
  TypeMapContext ctx tMap cal where
    tmCtxFind _ = tmFind
    tmCtxGet  _ = tmGet


-- data TypeMapContextKey ctx k = TypeMapContextKey

-- data TMKV tMap = forall tk . (TypeMapKey tk) => TMKV tk (TMValue tk)

-----------------------------------------------------------------------------
