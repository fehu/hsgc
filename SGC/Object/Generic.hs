-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Generic
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


module SGC.Object.Generic (

  GenericObject(..), TMVar(..)

, module Export

) where

import SGC.Object.Definitions as Export
import SGC.Object.SomeObject  as Export
import SGC.Object.Generic.TypeMap

import Data.Type.Bool
import Data.Proxy
import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

data GenericObject base (consts :: [*]) (vars :: [*]) = GenericObject{
    objBase   :: base,
    objConsts :: TMap consts,
    objVars   :: TMap vars
  }

type KnownGenericObject base m consts vars =
  ( TypeMapContext ObjectConstsCtx Id consts
  , TypeMapContext ObjectVarsCtx   m  vars
   ) =>
     GenericObject base consts vars

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

withVar' :: ( TypeMapKey k, TypeMapContext ObjectVarsCtx m vs, Typeable m ) =>
            (TMVar m (KeyValue k) -> m b) -> k -> GenericObject base cs vs -> m b
withVar' f k obj = f $ tmCtxGet ObjectVarsCtx (objVars obj) k

-----------------------------------------------------------------------------
