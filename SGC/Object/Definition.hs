-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Definition
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


module SGC.Object.Definition (

  Object(..), ObjectKey(..)

, HasConst, HasVar
, HasConst', HasVar'

, ObjectConst(..), ObjectVar(..)
, TMVar(..)

, ObjectValue(..)

-- * re-export

, TypeMapKey(..)


) where


import SGC.Object.TypeMap

import Data.Type.Bool
import Data.Proxy

-----------------------------------------------------------------------------

data Object (consts :: [*]) (vars :: [*]) = Object{
    objId     :: String,
    objConsts :: TMap consts,
    objVars   :: TMap vars
  }

-----------------------------------------------------------------------------

type family HasConst' obj c :: Bool
  where HasConst' (Object consts vars) c = TMContains consts c

type family HasVar' obj v :: Bool
  where HasVar' (Object consts vars) v = TMContains vars v

type HasConst obj c = (HasConst' obj c ~ True)
type HasVar   obj c = (HasVar'   obj c ~ True)

-----------------------------------------------------------------------------

type ObjectKey k = TypeMapKey k
type KeyValue k = TMValue k

-----------------------------------------------------------------------------


class (ObjectKey c, HasConst obj c) =>
  ObjectConst obj c where
    getConst :: obj -> c -> KeyValue c


class (ObjectKey v, HasVar obj v, Monad m) =>
  ObjectVar obj v m where
    readVar   :: obj -> v -> m (KeyValue v)
    writeVar  :: obj -> v -> KeyValue v -> m ()
    updateVar :: obj -> v -> (KeyValue v -> KeyValue v) -> m ()


-----------------------------------------------------------------------------

data ObjectConstsCtx = ObjectConstsCtx
data ObjectVarsCtx   = ObjectVarsCtx

instance ( ObjectKey c
         , TMContains cs c ~ True
         , TypeMapContext ObjectConstsCtx cs (KeyValue c)
          ) =>
  ObjectConst (Object cs vs) c where getConst = tmCtxGet ObjectConstsCtx . objConsts

instance ( ObjectKey v, Monad m
         , TMContains vs v ~ True
         , TMValue v ~ TMVar (KeyValue v) m
         , TypeMapContext ObjectVarsCtx vs (TMVar (KeyValue v) m)
         , TypeMap vs
          ) =>
  ObjectVar (Object cs vs) v m where
    readVar   = (tmVarRead .)   . tmCtxGet ObjectVarsCtx . objVars
    writeVar  = (tmVarWrite .)  . tmCtxGet ObjectVarsCtx . objVars
    updateVar = (tmVarUpdate .) . tmCtxGet ObjectVarsCtx . objVars

-----------------------------------------------------------------------------

data TMVar v m = TMVar {
    tmVarRead   :: m v
  , tmVarWrite  :: v -> m ()
  , tmVarUpdate :: (v -> v) -> m ()
  }

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (ObjectKey k, Monad m) =>
  ObjectValue obj k m where
    readValue :: obj -> k -> m (KeyValue k)


-----------------------------------------------------------------------------

data ObjectValueT = ObjectValueConst | ObjectValueVar

type family ObjValueT obj v :: ObjectValueT
  where ObjValueT obj v = ObjValueT' (HasConst' obj v) (HasVar' obj v)

objValueT :: obj -> v -> Proxy (ObjValueT obj v)
objValueT _ _ = Proxy

type family ObjValueT' (hasConst :: Bool) (hasVar :: Bool) :: ObjectValueT
  where ObjValueT' True False = ObjectValueConst
        ObjValueT' False True = ObjectValueVar


class (TypeMapKey k, Monad m) =>
  ObjectValue' obj k m (t :: ObjectValueT) where
    readValue' :: Proxy t -> obj -> k -> m (KeyValue k)

instance ( ObjectKey k, Monad m
         , ObjectValue' obj k m (ObjValueT obj k)
          ) =>
  ObjectValue obj k m where readValue obj k = readValue' (objValueT obj k) obj k


instance ( ObjectKey k, Monad m
         , ObjectConst obj k, HasVar' obj k ~ False
        --  , TMValue k ~ v
          ) =>
  ObjectValue' obj k m ObjectValueConst where
    readValue' _ obj = return . getConst obj

instance ( ObjectKey k, Monad m
         , ObjectVar obj k m, HasConst' obj k ~ False
          ) =>
  ObjectValue' obj k m ObjectValueVar where
    readValue' _ = readVar

-----------------------------------------------------------------------------
