{-# LANGUAGE RankNTypes, FlexibleInstances, DeriveDataTypeable, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Tst
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


module SGC.ObjectOld.Tst (

) where

import SGC.Measures
import SGC.Measures.SI
import SGC.ObjectOld
import SGC.Utils

import Data.Typeable
import Data.Function ( on )
import Data.List ( intercalate )

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



-----------------------------------------------------------------------------

absSpeed s = scalarUnit s Speed (5 :: Float)
time s = scalarUnit s Time (10 :: Float)
dist s = uMultiply s (absSpeed s) (time s)


data Vec2 n = Vec2 n n deriving Show

--fvec2_1 f (Vec2 n n) =
fvec2_2 f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)

--type instance IsVector Vec2 = True
instance (Floating n) =>
    Num (Vec2 n) where
        (+) = fvec2_2 (+)
        (*) = fvec2_2 (*)
        abs (Vec2 x y) = let a = x*x + y*y
                         in Vec2 a a
        negate (Vec2 x y) = Vec2 (-x) (-y)
        signum = id
        fromInteger i = let x = fromInteger i / 2
                        in Vec2 (sqrt x) (sqrt x)

instance (Fractional n) =>
    VectorOps Vec2 n where vMultiplyConst' (Vec2 x y) c = Vec2 (c*x) (c*y)
                           vDivideConst'   (Vec2 x y) c = Vec2 (x/c) (y/c)

--instance Num (Vec2 n)

speed s = vectorUnit s Speed (Vec2 2.3 5.2 :: Vec2 Float)
dist2 s = uMultiply s (speed s) (time s)


absSpeed' s = uDivide s (dist s) (time s)
speed' s = uDivide s (dist2 s) (time s)


foo s = withUnitSystem s $ do
          v <- mkVector Speed (Vec2 1 (-1) :: Vec2 Float)
          t <- mkScalar Time (5 :: Float)
          v ~*~ t


-----------------------------------------------------------------------------

data PointForm = PointForm deriving (Show, Typeable)

instance ObjectForm PointForm

data Point c = Point { pId      :: ObjectId
                     , pMass    :: (MassUnit USystem ~ u) => u UNum
                     , pCfg     :: c
                     }
             deriving Typeable

instance Show (Point c) where show Point {pId=id} = show id
instance Eq   (Point c) where p1 == p2 = pId p1 == pId p2
instance Ord  (Point c) where compare = compare `on` pId


instance (UniverseConfig c) =>
    Object (Point c) c where
        objId x _   = pId x
        objMass _   = pMass
        objForm _ _ = SomeObjForm PointForm


data GravitySystem c = GravitySystem { sysId     :: SystemId
                                     , sysObjPos :: (PositionUnit USystem ~ pos) =>
                                                  Map (SomeObject c) (pos (UVec UNum))
                                     }

instance Show (GravitySystem c) where show s = show (sysId s) ++ ": "
                                            ++ intercalate ", " (
                                                  map (\(o,p) -> show o
                                                              ++ " -> "
                                                              ++ show p
                                                      )
                                                      (Map.assocs $ sysObjPos s)
                                              )

instance (Ord ObjectId) =>
    ObjectSystem (GravitySystem c) c where
        systemId = DoWith sysId
        systemObjects = DoWith (Map.keys . sysObjPos)
        positionOf x  = DoWith (Map.lookup x . sysObjPos)

newGObjSystem :: (PositionUnit USystem ~ pos) =>
    SystemId -> [(SomeObject c, pos (UVec UNum))] -> DoWith c (GravitySystem c)

newGObjSystem sId objs = return $ GravitySystem sId (Map.fromList objs)


-----------------------------------------------------------------------------

data TCfg = TCfg deriving (Show, Typeable)

newtype ObjId = ObjId String deriving (Show, Eq, Ord)
newtype SysId = SysId String deriving (Show, Eq, Ord)

data NZero = NZero
data NOne a = NOne a

type DZero   = NZero
type DOne    = NOne NZero
type DTwo    = NOne DOne
type DThree  = NOne DTwo

instance UniverseConfig TCfg where
    type USystem    = SI
    type UNum       = Float
    type UVec n     = Vec2 n
    type UVectorDim = DTwo

    type ObjectId = ObjId
    type SystemId = SysId

    uSystem = return SI


mkObj :: (UniverseConfig c, Object a c) => (c -> a) -> DoWith c a
mkObj = DoWith

someObj :: (UniverseConfig c, Object a c, Typeable a, Ord a, Show a) =>
        (c -> a) -> DoWith c (SomeObject c)
someObj f = DoWith $ \c -> SomeObj c (f c)

bar = withUniverseConfig TCfg $ do
    usys <- uSystem

    let xv = withUnitSystem usys $ mkScalar Mass (5 :: UNum)
    let xp = withUnitSystem usys $ mkVector Position (Vec2 2 5 :: UVec UNum)
    x <- someObj $ Point (ObjId "x") xv

    let (yv, yp) = withUnitSystem usys $ do m <- mkScalar Mass (5e4 :: UNum)
                                            p <- mkVector Position (Vec2 0 0 :: UVec UNum)
                                            return (m, p)
    y <- someObj $ Point (ObjId "y") yv

    newGObjSystem (SysId "sys") [(x, xp), (y, yp)]


