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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}


module SGC.Object.InternalNew.Generic where

import SGC.Object.Definitions as Export
import SGC.Object.SomeObject  as Export

import SGC.Object.InternalNew.TypeSet
import SGC.Object.InternalNew.TypeMap as TM

import TypeNum.TypeFunctions (Contains)

import Data.Functor.Identity
import Data.Kind ( type (*))

import Control.Applicative ((<|>))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

infixl 5 :::
infixl 6 ::$


data GenericObject base (m :: * -> *) (consts :: TypeSet) (vars :: TypeSet) =
   GenericObject
    { objBase   :: base
    , objConsts :: TMap Identity consts
    , objVars   :: TMap (ObjVar m) vars
    }

newGenericObject :: ( ObjectValuesList2TMap consts
                    , ObjectValuesList2TMap vars
                    , Wrap (ObjVar m)
                    ) =>
                    base
                 -> ObjectValuesList ObjectConstant consts
                 -> ObjectValuesList (ObjectVariable m) vars
                 -> GenericObject base m consts vars

data ObjectValuesList (t :: ObjectValueAccessType) (keys :: TypeSet) :: * where
   ObjectVals :: ObjectValuesList ObjectConstant SetNil
   ObjectVars :: ObjectValuesList (ObjectVariable varm) SetNil
   (:::) :: ( InsertOrdering k keys' ~ keys
            , TypeMapPut keys' k
            , ObjectValuesList2TMap keys'
            ) =>
            ObjectValuesList t keys' -> ObjectKV k
         -> ObjectValuesList t keys

data ObjectKV k = (::$) k (TM.KeyValue k)

-----------------------------------------------------------------------------

newGenericObject base cs vs = GenericObject base (tMap cs) (tMap vs)

data ObjectValueAccessType = ObjectConstant | ObjectVariable (* -> *)


class ObjectValuesList2TMap (keys :: TypeSet) where
  tMap :: (Wrap wrap) => ObjectValuesList t keys -> TMap wrap keys

instance ObjectValuesList2TMap SetNil where tMap _ = tmEmpty
instance (ObjectValuesList2TMap t, InsertOrdering h t ~ SetCons h t) =>
   ObjectValuesList2TMap (SetCons h t) where
    tMap (t ::: (k ::$ v)) = tmPut' k v $ tMap t

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type instance HasConst' (GenericObject base varm consts vars) c = Contains c consts
type instance HasVar'   (GenericObject base varm consts vars) v = Contains v vars


data ObjVar m v = ObjVar {
  oVarRead   :: m v,
  oVarWrite  :: v -> m (),
  oVarUpdate :: (v -> v) -> m ()
  }

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


instance (ObjectKey c, Contains c cs ~ True, TypeMap Identity cs) =>
  ObjectConst (GenericObject base varm cs vs) c where
    getConst k = runIdentity . tmGet k . objConsts

instance (ObjectKey v, Contains v vs ~ True, Monad m, TypeMap (ObjVar m) vs) =>
  ObjectVar (GenericObject base m cs vs) v m
    where
      readVar   k = withVar k oVarRead
      writeVar  k = withVar k . flip oVarWrite
      updateVar k = withVar k . flip oVarUpdate

withVar k f = f . tmGet k . objVars

instance CreateSomeObject (GenericObject base m cs vs) base where
    someObject = flip SomeObject objBase

instance (TypeMap Identity cs) =>
  ObjectMayHaveConst (GenericObject base m cs vs) where
    tryGetConst k = fmap runIdentity . tmFind k . objConsts

instance (TypeMap Identity cs, TypeMap (ObjVar m) vs, Monad m) =>
  ObjectMayHave (GenericObject base m cs vs) m where
    tryReadVar   k = withVar' k oVarRead
    tryWriteVar  k = withVar' k . flip oVarWrite
    tryUpdateVar k = withVar' k . flip oVarUpdate
    tryReadValue k = (<|>) <$> fmap return . tryGetConst k
                           <*> tryReadVar k

withVar' k f = fmap f . tmFind k . objVars

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

objSelect :: (TypeMapSelect cs' cs, TypeMapSelect vs' vs) =>
             TSet cs' -> TSet vs' -> GenericObject base m cs vs
          -> Maybe (GenericObject base m cs' vs')

objSelect cs' vs' (GenericObject base cs vs) =
  GenericObject base <$> tmSelect cs' cs <*> tmSelect vs' vs


-----------------------------------------------------------------------------



-----------------------------------------------------------------------------
