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

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}

module SGC.Universe where

import SGC.Object

import PhyQ
import PhyQ.SI

import Data.Typeable (Typeable, gcast)

-----------------------------------------------------------------------------

class (Monad m, Num v) =>
  System sys m vec v | sys -> vec, vec -> v
    where
      sysGetObjects     :: sys -> m [SomeObject' (HasPosition sys vec)]
      sysSetObjects     :: sys -> [SomeObject' (HasPosition sys vec)] -> m ()
      sysUpdateObjects  :: sys
                        -> (   [SomeObject' (HasPosition sys vec)]
                            -> [SomeObject' (HasPosition sys vec)]  )
                        -> m ()

      sysUpdateObjects sys f = sysSetObjects sys =<< f <$> sysGetObjects sys


-----------------------------------------------------------------------------

type AppliedForce usys vec = Measurable Force vec

data Interaction usys vec sys obj = Interaction {
  interaction :: sys -> obj -> obj -> AppliedForce usys vec
}

data Interact usys sys vec = forall obj . Typeable obj =>
                             Interact (Interaction usys vec sys obj)

interact2 :: (Typeable obj, Num vec, Ord vec) =>
             [Interact usys sys vec] -> sys -> obj -> obj -> AppliedForce usys vec
interact2 (Interact f' : is) sys x y =
  case gcast f' of Just f -> interaction f sys x y  $+ interact2 is sys x y
                   _      -> interact2 is sys x y
interact2 [] _ _ _ = Measurable $ const (measure 0)

-----------------------------------------------------------------------------

data ApplyForce obj m usys vec v =
     ApplyForce (obj -> AppliedForce usys vec -> v -> m obj)


objsInteract :: (Monad m, Eq obj, Typeable obj, Num vec, Ord vec) =>
                ApplyForce obj m usys vec v
             -> [Interact usys sys vec]
             -> sys
             -> [obj]
             -> v
             -> m [obj]
objsInteract (ApplyForce applyF) is sys objs time = sequence $ do
  x  <- objs
  let fs = do y <- objs
              if x == y then []
                else return $ interact2 is sys x y
      f = foldr measuresSum (measurable 0) fs
  return $ applyF x f time


-----------------------------------------------------------------------------
