-----------------------------------------------------------------------------
--
-- Module      :  SGC.Universe
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module SGC.Universe where

import SGC.Object

import PhyQ
import PhyQ.SI

import Data.Typeable (Typeable, cast, gcast)

-----------------------------------------------------------------------------

class (Monad m) => System sys m
  where
    sysGetObjects     :: sys -> m [AnyObject id m]
    sysSetObjects     :: sys -> [AnyObject id m] -> m ()
    sysUpdateObjects  :: sys
                      -> (   [AnyObject id m]
                          -> [AnyObject id m]  )
                      -> m ()

    sysUpdateObjects sys f = sysSetObjects sys =<< f <$> sysGetObjects sys

-----------------------------------------------------------------------------

type AppliedForce usys vec m = m (Measurable Force vec)

data Interaction usys vec sys id m = Interaction {
  interaction :: sys -> AnyObject id m -> AnyObject id m -> AppliedForce usys vec m
}

interact2 :: (Num vec, Ord vec, Typeable m, Monad m) =>
             [Interaction usys vec sys id m] -> sys
          -> AnyObject id m -> AnyObject id m -> AppliedForce usys vec m
interact2 (f' : is) sys x y = -- undefined
  case gcast f' of Just f -> ($+) <$> interaction f sys x y
                                  <*> interact2 is sys x y
                   _      -> interact2 is sys x y
interact2 [] _ _ _ = return $ measurable 0

-----------------------------------------------------------------------------

data ApplyForce obj m usys vec v =
     ApplyForce (obj -> AppliedForce usys vec m -> v -> m obj)



objsInteract :: (Num vec, Ord vec, Monad m, Typeable m, Ord id) =>
                ApplyForce (AnyObject id m) m usys vec v
             -> [Interaction usys vec sys id m]
             -> sys
             -> [AnyObject id m]
             -> v -- ^ time
             -> m [AnyObject id m]
objsInteract (ApplyForce applyF) is sys objs time = sequence $ do
  x  <- objs
  let fs = sequence $ do y <- objs
                         if x == y then []
                           else return $ interact2 is sys x y
  let f = foldr measuresSum (measurable 0) <$> fs
  return $ applyF x f time


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- class -- (Monad m) =>
--   CoortinateSystem sys vec v | sys -> vec, vec -> v -- , Num v
--     where

-----------------------------------------------------------------------------
