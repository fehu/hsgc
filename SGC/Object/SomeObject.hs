-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.SomeObject
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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module SGC.Object.SomeObject(

  ObjectValueType(..), Const, Var
, ObjectHas, Has, NoCtx

, SomeObject(..), HasConstraints
, CreateSomeObject(..)

, withObjectBase, withObject

-- * Internal

, MergeConstraints, MergeConstraints', ValueForEach

) where

import SGC.Object.Definitions

import TypeNum.TypeFunctions ( type (++) )

import GHC.Exts (Constraint)

-----------------------------------------------------------------------------

data ObjectValueType = Consts | Vars | Any

type Const = Consts
type Var = Vars

type ObjectHas obj m (t :: ObjectValueType) (ks :: [*]) =
               MergeConstraints (ValueForEach t obj ks m)

type Has (t :: ObjectValueType) (ks :: [*]) = '(t, ks)


class NoCtx a
instance NoCtx a

-----------------------------------------------------------------------------

data SomeObject (baseC :: * -> Constraint) m (has :: [(ObjectValueType, [*])]) =
  forall obj base . ( HasConstraints obj m has, baseC base ) =>
    SomeObject obj (obj -> base)

class CreateSomeObject obj base | obj -> base
    where
      someObject :: (HasConstraints obj m has, baseC base) =>
                    obj -> SomeObject baseC m has

-----------------------------------------------------------------------------

withObject :: (forall obj . HasConstraints obj m has => obj -> r)
           -> SomeObject c m has -> r
withObject f (SomeObject obj _) = f obj

withObjectBase :: (forall base . c base => base -> r)
               -> SomeObject c m has -> r
withObjectBase f (SomeObject obj getBase) = f $ getBase obj

-----------------------------------------------------------------------------

type family MergeConstraints (cs :: [Constraint]) :: Constraint
  where  MergeConstraints cs = MergeConstraints' cs ()

type family MergeConstraints' (cs :: [Constraint]) (acc :: Constraint) :: Constraint
  where MergeConstraints' (c ': cs) acc = (c, acc)
        MergeConstraints' '[] acc = acc

type family ValueForEach (t :: ObjectValueType) obj (ks :: [*]) m :: [Constraint]
  where ValueForEach t obj    '[]    m = '[]
        ValueForEach t obj (k ': ks) m = ValueConstraint t obj k m
                                         ': ValueForEach t obj ks m

type family ValueConstraint (t :: ObjectValueType) obj k m :: Constraint
  where ValueConstraint Consts obj k m = ObjectConst obj k
        ValueConstraint Vars   obj k m = ObjectVar   obj k m
        ValueConstraint Any    obj k m = ObjectValue obj k m

type HasConstraints obj m has = MergeConstraints (HasConstraints' obj m has)

type family HasConstraints' obj m (has :: [(ObjectValueType, [*])]) :: [Constraint]
  where
    HasConstraints' obj m '[] = '[]
    HasConstraints' obj m ('(t, ks) ': tks) = ValueForEach t obj ks m
                                           ++ HasConstraints' obj m tks

-----------------------------------------------------------------------------
