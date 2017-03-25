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

  HasConst, HasVar
, HasConst', HasVar'

, ObjectConst(..), ObjectVar(..)
-- , TMVar(..)

, ObjectValue(..), HasValue

-- * re-export

, TypeMapKey(..), ObjectKey, KeyValue


) where

import SGC.Object.Generic.TypeMap

import Data.Proxy

-----------------------------------------------------------------------------

type family HasConst' obj c :: Bool
type family HasVar' obj v :: Bool

type HasConst obj c = (HasConst' obj c ~ True)
type HasVar   obj c = (HasVar'   obj c ~ True)

-----------------------------------------------------------------------------

type ObjectKey k = TypeMapKey k
type KeyValue k = TMValue k

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
  ObjectValue obj k m where
    readValue :: k -> obj -> m (KeyValue k)

type HasValue obj k m = ObjectValue obj k m

-----------------------------------------------------------------------------
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
  ObjectValue obj k m where readValue k obj = readValue' (objValueT obj k) obj k


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
