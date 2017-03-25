-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Generic.Create
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module SGC.Object.Generic.Test where

import SGC.Object.Generic
import SGC.Object.Generic.Create
import SGC.Object.Generic.TypeMap

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

data A = A deriving (Eq)
instance TypeMapKey A where type TMValue A = String

foo :: SomeObject Show IO '[Has Consts '[A]] -> String
foo (SomeObject obj _) =  getConst A obj
-- foo = withObject $ getConst A

bar :: SomeObject Show IO '[Has Vars '[A]] -> IO String
bar = withObject $ readVar A

baz :: SomeObject Show IO '[Has Any '[A]] -> IO String
baz = withObject $ readValue A

baf :: SomeObject Show IO '[Has Any '[A]] -> String
baf = withObjectBase show

-----------------------------------------------------------------------------

tm :: TMap '[A]
tm = undefined

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

-----------------------------------------------------------------------------
