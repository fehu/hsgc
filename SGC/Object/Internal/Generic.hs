-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Internal.Generic
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module SGC.Object.Internal.Generic (

  GenericObject(..), TMVar(..)

, ObjectValuesList(..), ObjectKV(..)
, newGenericObject

, module Export

) where

import SGC.Object.Internal.Definitions as Export
import SGC.Object.Internal.SomeObject  as Export
import SGC.Object.Internal.TypeMap

import Data.Type.Bool
import Data.Proxy
import Data.Typeable (Typeable)

import TypeNum.TypeFunctions (type (++))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data GenericObject base (consts :: [*]) (vars :: [*]) = GenericObject{
    objBase   :: base,
    objConsts :: TMap consts,
    objVars   :: TMap vars
  }

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

infixl 5 :::
infixl 6 ::$

newGenericObject :: (ObjectValuesList2TMap consts, ObjectValuesList2TMap vars) =>
                    base
                 -> ObjectValuesList ObjectConstant consts
                 -> ObjectValuesList ObjectVariable vars
                 -> GenericObject base consts vars
newGenericObject base cs vs = GenericObject base (tMap cs) undefined

-----------------------------------------------------------------------------

data ObjectValueAccessType = ObjectConstant | ObjectVariable

data ObjectKV k = (::$) k (KeyValue k)

data ObjectValuesList (t :: ObjectValueAccessType) (kl :: [*]) :: * where
   ObjectVals :: ObjectValuesList ObjectConstant '[]
   ObjectVars :: ObjectValuesList ObjectVariable '[]
   (:::) :: ObjectValuesList t kl -> ObjectKV k -> ObjectValuesList t (k ': kl)

-----------------------------------------------------------------------------

class ObjectValuesList2TMap (kl :: [*]) where
  tMap :: ObjectValuesList t kl -> TMap kl

instance ObjectValuesList2TMap '[] where tMap _ = tmEmpty
instance ( TMChange ks k ~ TypeMapGrowth, ObjectKey k
         , TMKeyType k ~ TypeMapKeyUser
         , ObjectValuesList2TMap ks
          ) =>
  ObjectValuesList2TMap (k ': ks) where
    tMap (t ::: (k ::$ v)) = tmUpdate (tMap t) k (TMSetValue v)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type instance HasConst' (GenericObject base consts vars) c = TMContains consts c
type instance HasVar'   (GenericObject base consts vars) v = TMContains vars v


-----------------------------------------------------------------------------

data TMVar m v = TMVar {
    tmVarRead   :: m v
  , tmVarWrite  :: v -> m ()
  , tmVarUpdate :: (v -> v) -> m ()
  }

-----------------------------------------------------------------------------

newtype Id a = Id a
fromId (Id x) = x

instance Functor     Id where fmap f  = Id . f . fromId
instance Applicative Id where pure    = Id
                              (<*>) f = Id . fromId f . fromId
instance Monad       Id where (>>=) x = ($ fromId x)

-----------------------------------------------------------------------------


data ObjectConstsCtx = ObjectConstsCtx
data ObjectVarsCtx   = ObjectVarsCtx


instance ( ObjectKey c, TMContains cs c ~ True
         , TypeMapContext ObjectConstsCtx Id cs
          ) =>
  ObjectConst (GenericObject base cs vs) c
    where getConst v = fromId . flip (tmCtxGet ObjectConstsCtx) v . objConsts

instance ( ObjectKey v, Monad m, Typeable m
         , TMContains vs v ~ True
         , TypeMapContext ObjectVarsCtx (TMVar m) vs
         , TypeMap vs
          ) =>
  ObjectVar (GenericObject base cs vs) v m where
    readVar       = withVar' tmVarRead
    writeVar  k v = withVar'  (`tmVarWrite` v) k
    updateVar k f =  withVar' (`tmVarUpdate` f) k

withVar' :: ( ObjectKey k, TypeMapContext ObjectVarsCtx m vs, Typeable m ) =>
            (TMVar m (KeyValue k) -> m b) -> k -> GenericObject base cs vs -> m b
withVar' f k obj = f $ tmCtxGet ObjectVarsCtx (objVars obj) k

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance (TypeMap cs, TypeMap vs) =>
  CreateSomeObject (GenericObject base cs vs) base where
    someObject obj = SomeObject obj objBase

-----------------------------------------------------------------------------
