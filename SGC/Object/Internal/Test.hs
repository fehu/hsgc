-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Internal.Create
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module SGC.Object.Internal.Test where

import SGC.Object.Internal.Generic

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

data A = A deriving (Eq)
instance ObjectKey A where type KeyValue A = String

foo :: SomeObject Show IO '[Has Consts '[A]] -> String
foo (SomeObject obj _) =  getConst A obj
-- foo = withObject $ getConst A

bar :: SomeObject Show IO '[Has Vars '[A]] -> IO String
bar = withObject $ readVar A

bar' :: String -> SomeObject Show IO '[Has Vars '[A]] -> IO ()
bar' s = withObject $ writeVar A s

baz :: SomeObject Show IO '[Has Any '[A]] -> IO String
baz = withObject $ readValue A

baf :: SomeObject Show IO '[Has Any '[A]] -> String
baf = withObjectBase show

-----------------------------------------------------------------------------

obj1 :: GenericObject String '[A] '[]
obj1 = undefined

obj1' :: SomeObject Show m '[]
obj1' = someObject obj1

obj1'' :: SomeObject Show m '[Has Consts '[A]]
obj1'' = someObject obj1

obj1''' :: (Monad m) => SomeObject Show m '[Has Any '[A]]
obj1''' = someObject obj1



obj2 :: GenericObject String '[] '[A]
obj2 = undefined

obj2' :: SomeObject Show m '[]
obj2' = someObject obj2

obj2'' :: (Monad m, Typeable m) => SomeObject Show m '[Has Vars '[A]]
obj2'' = someObject obj2

obj2''' :: (Monad m, Typeable m) => SomeObject Show m '[Has Any '[A]]
obj2''' = someObject obj2

-----------------------------------------------------------------------------

data B = B deriving (Eq)
instance ObjectKey B where type KeyValue B = Int

obj :: GenericObject String '[A] '[B]
obj = newGenericObject "Test" (ObjectVals ::: A::$"Test")
                              (ObjectVars ::: B::$1)

-----------------------------------------------------------------------------
