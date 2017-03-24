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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module SGC.Object where

import PhyQ

import Data.Typeable (Typeable)

import GHC.Exts (Constraint)

-----------------------------------------------------------------------------

class AnyObject obj where objId :: obj -> String

data SomeObject = forall obj . (Typeable obj, AnyObject obj) => SomeObject obj

data SomeObject' (c :: * -> Constraint) =
  forall obj . (Typeable obj, AnyObject obj, c obj) =>
    SomeObject' obj

fromSomeObject' :: (forall obj . (Typeable obj, AnyObject obj, c obj) => obj -> x)
                -> SomeObject' c
                -> x
fromSomeObject' f (SomeObject' obj) = f obj

-----------------------------------------------------------------------------

type Measure a v q = a -> Measurable q v

class HasMass v a where
  objMass :: Measure a v Mass

class HasPosition sys vec a where
  objPosition :: sys -> Measure a vec Position
  objSpeed    :: sys -> Measure a vec Speed

objDistance ::  (HasPosition sys vec a, Ord vec, Num vec) =>
                sys -> a -> a -> Measurable Position vec
objDistance sys x y = let px = objPosition sys x
                          py = objPosition sys y
                      in py $- px

-----------------------------------------------------------------------------

class (HasPosition sys vec a, HasMass v a) =>
  MaterialPoint' sys vec v a | vec -> v
type MaterialPoint sys vec v = SomeObject' (MaterialPoint' sys vec v)

instance HasPosition sys vec (MaterialPoint sys vec v) where
  objPosition sys = fromSomeObject' $ objPosition sys
  objSpeed    sys = fromSomeObject' $ objSpeed sys

instance HasMass v (MaterialPoint sys vec v) where
  objMass = fromSomeObject' objMass

-----------------------------------------------------------------------------

-- class HasPosition sys vec a where
--   objPosition :: sys -> Measure a vec Position
--   objSpeed    :: sys -> Measure a vec Speed



-----------------------------------------------------------------------------
