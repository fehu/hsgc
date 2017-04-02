-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module SGC.Object (

  ObjectId(..)
, onObjId, objectsEqId, objectsOrdId

, AnyObject(..)

, module Export

) where

import SGC.Object.Internal.Generic as Export

import SGC.Object.Internal.TypeMap

import TypeNum.TypeFunctions (type (++))

import Data.Typeable (Typeable, Proxy(..))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (Ord id) => ObjectId id obj | obj -> id
  where objId :: obj -> id

onObjId :: (ObjectId id obj1, ObjectId id obj2) => (id -> id -> r)
                                                -> obj1 -> obj2 -> r
onObjId f x y = f (objId x) (objId y)

objectsEqId :: (ObjectId id obj1, ObjectId id obj2) => obj1 -> obj2 -> Bool
objectsEqId = onObjId (==)

objectsOrdId :: (ObjectId id obj1, ObjectId id obj2) => obj1 -> obj2 -> Ordering
objectsOrdId = onObjId compare

instance (ObjectId id base) => ObjectId id (GenericObject base cs vs) where
  objId = objId . objBase

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data AnyObject id m = forall base cs vs . ( ObjectId id base, Typeable base
                                          , TypeMapContext ObjectConstsCtx cs
                                          , TypeMapContext (ObjectVarsCtx m) vs
                                          ) =>
                      AnyObject (GenericObject base cs vs)

instance ObjectMayHaveConst (AnyObject id m) where
  tryGetConst k (AnyObject obj) = tryGetConst k obj

instance (Monad m, Typeable m) => ObjectMayHave (AnyObject id m) m where
  tryReadVar   k   (AnyObject obj) = tryReadVar k obj
  tryWriteVar  k v (AnyObject obj) = tryWriteVar k v obj
  tryUpdateVar k f (AnyObject obj) = tryUpdateVar k f obj
  tryReadValue k   (AnyObject obj) = tryReadValue k obj

instance (Ord id) => ObjectId id (AnyObject id m) where
  objId (AnyObject obj) = objId obj
instance (Ord id) => Eq  (AnyObject id m) where (==) = objectsEqId
instance (Ord id) => Ord (AnyObject id m) where compare = objectsOrdId


-----------------------------------------------------------------------------

castAnyObject :: ( TypeSubMap (MkVarKeys vars m), TypeSubMap (MkConstKeys consts)
                 , TypeMap (MkConstKeys consts), TypeMap (MkVarKeys vars m)
                 , Typeable (MkConstKeys consts), Typeable (MkVarKeys vars m)
                --  , obj ~ GenericObject base (MkConstKeys cs') (MkVarKeys vs' m)
                --  , MergeConstraints' (ValueForEach Consts obj (MkConstKeys cs') m) ()
                --  , MergeConstraints' (ValueForEach Vars   obj (MkVarKeys vs' m) m) ()
                  ) =>
                 Proxy (consts :: [*]) -> Proxy (vars :: [*])
              -> AnyObject id m
              -> Maybe (SomeObject (ObjectId id) m '[Has Consts consts, Has Vars vars])
castAnyObject hasCs hasVs o@(AnyObject obj') = undefined
  -- let mbObj = buildSubObject hasCs hasVs (proxyM o) obj' -- gSubObject
  -- in someObject <$> mbObj


-- castAnyObject :: Proxy (has :: [(ObjectValueType, [*])]) -> AnyObject id m
--               -> Maybe (SomeObject (ObjectId id) m has)
-- castAnyObject has o@(AnyObject obj') = let mbObj = gSubObject (separateConstValues has)
--                                                               (separateVarValues has)
--                                                             -- (separateValues has)
--                                                               (proxyM o)
--                                                               obj'
--                                         in someObject <$> mbObj


-- separateConstValues :: Proxy has -> Proxy (SeparateConstValues has)
-- separateConstValues = const Proxy
-- type family SeparateConstValues (has :: [(ObjectValueType, [*])]) :: [*]
--   where SeparateConstValues ('(Const, vs) ': has) = vs ++ SeparateConstValues has
--         SeparateConstValues (other ': has)        = SeparateConstValues has
--         SeparateConstValues '[]                   = '[]
--
--
-- separateVarValues :: Proxy has -> Proxy (SeparateVarValues has)
-- separateVarValues = const Proxy
-- type family SeparateVarValues (has :: [(ObjectValueType, [*])]) :: [*]
--   where SeparateVarValues ('(Var, vs) ': has) = vs ++ SeparateVarValues has
--         SeparateVarValues (other ': has)      = SeparateVarValues has
--         SeparateVarValues '[]                 = '[]
--
--
-- separateAnyValues :: Proxy has -> Proxy (SeparateAnyValues has)
-- separateAnyValues = const Proxy
-- type family SeparateAnyValues (has :: [(ObjectValueType, [*])]) :: [*]
--   where SeparateAnyValues ('(Any, vs) ': has) = vs ++ SeparateAnyValues has
--         SeparateAnyValues (other ': has)      = SeparateAnyValues has
--         SeparateAnyValues '[]                 = '[]

proxyM :: AnyObject id m -> Proxy m
proxyM = const Proxy

-----------------------------------------------------------------------------
