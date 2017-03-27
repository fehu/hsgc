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

module SGC.Object (

  ObjectId(..)
, onObjId, objectsEqId, objectsOrdId

, module Export

) where

import SGC.Object.Internal.Generic as Export

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class (Ord id) => ObjectId obj id | obj -> id
  where objId :: obj -> id

onObjId :: (ObjectId obj1 id, ObjectId obj2 id) => (id -> id -> r)
                                                -> obj1 -> obj2 -> r
onObjId f x y = f (objId x) (objId y)

objectsEqId :: (ObjectId obj1 id, ObjectId obj2 id) => obj1 -> obj2 -> Bool
objectsEqId = onObjId (==)

objectsOrdId :: (ObjectId obj1 id, ObjectId obj2 id) => obj1 -> obj2 -> Ordering
objectsOrdId = onObjId compare

instance (ObjectId base id) => ObjectId (GenericObject base cs vs) id where
  objId = objId . objBase

-----------------------------------------------------------------------------
