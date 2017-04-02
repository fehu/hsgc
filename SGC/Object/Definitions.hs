-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Definitions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SGC.Object.Definitions (

  ObjectKey(..), TypeKey(..)

, HasConst, HasVar
, HasConst', HasVar'

, ObjectConst(..), ObjectVar(..)

, ObjectValue(readValue), HasValue

, ObjectMayHaveConst(..), ObjectMayHave(..)


) where

import SGC.Object.InternalNew.TypeMap (TypeKey(..))

import Data.Proxy
import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

-- type family HasConst' obj c               :: Bool
-- type family HasVar'   obj v (m :: * -> *) :: Bool
--
-- type HasConst obj c = (HasConst' obj c ~ True)
-- type HasVar obj v m = (HasVar' obj v m ~ True)

type family HasConst' obj c :: Bool
type family HasVar'   obj v :: Bool

type HasConst obj c = (HasConst' obj c ~ True)
type HasVar obj v   = (HasVar' obj v   ~ True)

-----------------------------------------------------------------------------

type ObjectKey k = TypeKey k

-----------------------------------------------------------------------------

class (ObjectKey c, HasConst obj c) =>
  ObjectConst obj c where
    getConst :: c -> obj -> KeyValue c


class (ObjectKey v, HasVar obj v, Monad m) =>
  ObjectVar obj v m where
    readVar   :: v -> obj -> m (KeyValue v)
    writeVar  :: v -> KeyValue v -> obj -> m ()
    updateVar :: v -> (KeyValue v -> KeyValue v) -> obj -> m ()

-----------------------------------------------------------------------------

class (ObjectKey k, Monad m) =>
  ObjectValue obj k (m :: * -> *) where
    readValue :: k -> obj -> m (KeyValue k)

type HasValue obj k m = ObjectValue obj k m

-----------------------------------------------------------------------------

class ObjectMayHaveConst obj where
  tryGetConst  :: (ObjectKey c) => c -> obj -> Maybe (KeyValue c)

class (ObjectMayHaveConst obj) =>
  ObjectMayHave obj m where
    tryReadVar   :: (ObjectKey v) => v -> obj -> Maybe (m (KeyValue v))
    tryWriteVar  :: (ObjectKey v) => v -> KeyValue v -> obj -> Maybe (m ())
    tryUpdateVar :: (ObjectKey v) => v -> (KeyValue v -> KeyValue v) -> obj -> Maybe (m ())
    tryReadValue :: (ObjectKey k) => k -> obj -> Maybe (m (KeyValue k))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data ObjectValueT = ObjectValueConst | ObjectValueVar

type family ObjValueT obj v (m :: * -> *) :: ObjectValueT
  where ObjValueT obj v m = ObjValueT' (HasConst' obj v) (HasVar' obj v)

objValueT :: obj -> v -> Proxy m -> Proxy (ObjValueT obj v m)
objValueT _ _ _ = Proxy

type family ObjValueT' (hasConst :: Bool) (hasVar :: Bool) :: ObjectValueT
  where ObjValueT' True False = ObjectValueConst
        ObjValueT' False True = ObjectValueVar


class (ObjectKey k, Monad m) =>
  ObjectValue' obj k m (t :: ObjectValueT) where
    readValue' :: Proxy t -> obj -> k -> m (KeyValue k)

instance ( ObjectKey k, Monad m
         , ObjectValue' obj k m (ObjValueT obj k m)
          ) =>
  ObjectValue obj k m where readValue k obj = readValue'' undefined obj k

readValue'' :: (ObjectValue' obj k m t, ObjValueT obj k m ~ t) =>
               Proxy t -> obj -> k -> m (KeyValue k)
readValue'' = readValue'

instance ( ObjectKey k, Monad m
         , ObjectConst obj k, HasVar' obj k ~ False
          ) =>
  ObjectValue' obj k m ObjectValueConst where
    readValue' _ obj = return . flip getConst obj

instance ( ObjectKey k, Monad m
         , ObjectVar obj k m, HasConst' obj k ~ False
          ) =>
  ObjectValue' obj k m ObjectValueVar where
    readValue' _ = flip readVar

-----------------------------------------------------------------------------
