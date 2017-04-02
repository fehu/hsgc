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

-- {-# OPTIONS_GHC -fprint-explicit-kinds #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module SGC.Object.Internal.Generic (

  GenericObject(..), TMVar(..), GObject

, ObjectValuesList(..), ObjectKV(..)
, newGenericObject

, module Export

-- * Internal

, ObjectConstsCtx(..), ObjectVarsCtx(..), MkVarKeys, MkConstKeys -- TODO: hide on re-export
, gSubObject
-- , BuildSubObject(..)

) where

import SGC.Object.Definitions as Export
import SGC.Object.SomeObject  as Export
import SGC.Object.Internal.TypeMap

import Data.Type.Bool
import Data.Proxy
import Data.Typeable -- (Typeable)
import Data.Maybe (fromJust)

import Control.Applicative ( (<|>) )

import TypeNum.TypeFunctions -- (type (++))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data GenericObject base (consts :: [*]) (vars :: [*]) = GenericObject{
    objBase   :: base,
    objConsts :: TMap consts,
    objVars   :: TMap vars
  }

type GObject base (consts :: [*]) (vars :: [*]) m =
  GenericObject base (MkConstKeys consts) (MkVarKeys vars m)

type family MkConstKeys (cs :: [*]) :: [*] where
  MkConstKeys '[] = '[]
  MkConstKeys (c ': cs) = ContextMapKey ObjectConstsCtx c ': MkConstKeys cs

type family MkVarKeys (vs :: [*]) (m :: * -> *) :: [*] where
  MkVarKeys '[] m = '[]
  MkVarKeys (v ': vs) m = ContextMapKey (ObjectVarsCtx m) v ': MkVarKeys vs m

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

infixl 5 :::
infixl 6 ::$

newGenericObject :: (ObjectValuesList2TMap consts, ObjectValuesList2TMap vars) =>
                    base
                 -> ObjectValuesList ObjectConstant consts
                 -> ObjectValuesList ObjectVariable vars
                 -> GenericObject base consts vars
newGenericObject base cs vs = GenericObject base (tMap cs) (tMap vs)

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

type instance HasConst' (GenericObject base consts vars) c =
  TMContains consts (ContextMapKey ObjectConstsCtx c)

type instance HasVar' (GenericObject base consts vars) v m =
  TMContains vars (ContextMapKey (ObjectVarsCtx m) v)


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
data ObjectVarsCtx (m :: * -> *) = ObjectVarsCtx

instance (TypeMap tm) => TypeMapContext ObjectConstsCtx tm where
  type CtxKeyValue ObjectConstsCtx = Id
instance (TypeMap tm, Typeable m) =>
  TypeMapContext (ObjectVarsCtx m) tm where
    type CtxKeyValue (ObjectVarsCtx m) = m

instance ( ObjectKey c, TypeMap cs
         , HasConst (GenericObject base cs vs) c
          ) =>
  ObjectConst (GenericObject base cs vs) c
    where getConst v = fromId . flip (tmCtxGet ObjectConstsCtx) v . objConsts

instance ( ObjectKey v, Monad m, Typeable m, TypeMap vs
         , HasVar (GenericObject base cs vs) v m
          ) =>
  ObjectVar (GenericObject base cs vs) v m where
    readVar       = withVar tmVarRead
    writeVar  k v = withVar  (`tmVarWrite` v) k
    updateVar k f =  withVar (`tmVarUpdate` f) k

withVar :: ( ObjectKey k, Typeable m, TypeMap vs ) =>
            (TMVar m (KeyValue k) -> m b) -> k -> GenericObject base cs vs -> m b
withVar f k obj = f $ tmCtxGet ObjectVarsCtx (objVars obj) k

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance (TypeMap cs, TypeMap vs) =>
  CreateSomeObject (GenericObject base cs vs) base where
    someObject obj = SomeObject obj objBase

-----------------------------------------------------------------------------

instance (TypeMap cs) => ObjectMayHaveConst (GenericObject base cs vs) where
  tryGetConst c = fmap fromId . flip (tmCtxFind ObjectConstsCtx) c . objConsts

instance (TypeMap cs, TypeMap vs, Monad m, Typeable m) =>
  ObjectMayHave (GenericObject base cs vs) m where
    tryReadVar       = withVar' tmVarRead
    tryWriteVar  k v = withVar' (`tmVarWrite` v) k
    tryUpdateVar k v = withVar' (`tmVarUpdate` v) k
    tryReadValue k o = fmap return (tryGetConst k o) <|> tryReadVar k o


withVar' :: ( ObjectKey k, Typeable m, TypeMap vs ) =>
            (TMVar m (KeyValue k) -> m b) -> k -> GenericObject base cs vs -> Maybe (m b)
withVar' f k obj = f <$> tmCtxFind ObjectVarsCtx (objVars obj) k

-----------------------------------------------------------------------------

-- subObject :: ( TypeSubMap (MkConstKeys cs'), TypeSubMap (MkVarKeys vs' m)
--              , TypeMap cs, TypeMap vs
--               ) =>
--      Proxy cs' -> Proxy vs' -> Proxy m
--   -> GenericObject base cs vs
--   -> Maybe (SomeObject NoCtx m '[ Has Consts (MkConstKeys cs')
--                                 , Has Vars   (MkVarKeys vs' m)])
-- subObject cs' vs' m' obj = someObject <$> subObject'  cs' vs' m' obj

  -- cs <- subTMap (proxyMkConstKeys cs') $ objConsts obj
  -- vs <- subTMap (proxyMkVarKeys m' vs') $ objVars obj
  -- return . someObject $ GenericObject (objBase obj) cs vs
--
-- subObject' :: ( TypeSubMap (MkConstKeys cs'), TypeSubMap (MkVarKeys vs' m)
--              , TypeMap cs, TypeMap vs
--              , obj ~ GenericObject base (MkConstKeys cs') (MkVarKeys vs' m)
--             --  , MergeConstraints' ( ValueForEach Consts obj (MkConstKeys cs') m ++
--             --                        ValueForEach Vars   obj (MkVarKeys vs' m) m)
--             --                     ()
--               ) =>
--      Proxy cs' -> Proxy vs' -> Proxy m
--   -> GenericObject base cs vs
--   -> Maybe obj
--
-- subObject' cs' vs' m' obj =  undefined


gSubObject :: ( TypeSubMap (MkConstKeys cs')
              , TypeSubMap (MkVarKeys vs' m)
              , TypeMap cs, TypeMap vs
              , obj ~ GenericObject base (MkConstKeys cs') (MkVarKeys vs' m)
              -- , MergeConstraints' (ValueForEach Consts obj (MkConstKeys cs') m) ()
              -- , MergeConstraints' (ValueForEach Vars   obj (MkVarKeys vs' m) m) ()
              , ObjectHas obj m Consts cs', ObjectHas obj m Vars   vs'
                ) =>
     Proxy cs' -> Proxy vs' -> Proxy m
  -> GenericObject base cs vs
  -> Maybe obj -- (GenericObject base (MkConstKeys cs') (MkVarKeys vs' m)) -- (GObject base cs' vs' m)

gSubObject cs' vs' m' obj = do
  cs <- subTMap (proxyMkConstKeys cs') $ objConsts obj
  vs <- subTMap (proxyMkVarKeys m' vs') $ objVars obj
  return $ GenericObject (objBase obj) cs undefined

proxyMkConstKeys ::                        Proxy (xs :: [*]) -> Proxy (MkConstKeys xs)
proxyMkVarKeys   :: Proxy (m :: * -> *) -> Proxy (xs :: [*]) -> Proxy (MkVarKeys xs m)
proxyMkConstKeys = const Proxy
proxyMkVarKeys _ = const Proxy


-----------------------------------------------------------------------------

-- class BuildSubObject (cs' :: [*]) (vs' :: [*]) (m :: * -> *) obj' obj where
--   buildSubObject :: Proxy cs' -> Proxy vs' -> Proxy m -> obj' -> Maybe obj
--
-- instance (ObjectHas obj m Consts cs', ObjectHas obj m Vars vs') =>
--   BuildSubObject cs' vs' m obj' obj where

class BuildSomeObject baseC (has :: [(ObjectValueType, [*])]) (m :: * -> *) where
  buildSomeObject :: ( baseC base, TypeMap cs, TypeMap vs ) =>
                     GenericObject base cs vs
                  -> Proxy has -> Proxy m
                  -> SomeObject baseC m has'
                  -> Maybe (SomeObject baseC m (has' ++ has))

-- instance BuildSomeObject baseC '[] m where
--   buildSomeObject obj _ _ = Just $ someObject obj
--
-- instance BuildSomeObject baseC ('(t, ks) ': hasMore) m where
--
--
-- class BuildSomeObject' baseC (t :: ObjectValueType) (ks :: [*]) (m :: * -> *) where
--   buildSomeObject' :: () =>
--                      GenericObject base cs vs
--                    -> t -> Proxy ks -> Proxy m
--                    -> Maybe (SomeObject baseC m has)

-- uniteSomeObjects :: SomeObject baseC m x -> SomeObject baseC m y
--                  -> SomeObject baseC m (x ++ y)
-- uniteSomeObjects (SomeObject x get) (SomeObject y _) =
--   SomeObject (fromJust $ cast x) get

-----------------------------------------------------------------------------
