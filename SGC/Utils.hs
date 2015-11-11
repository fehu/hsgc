-----------------------------------------------------------------------------
--
-- Module      :  SGC.Utils
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

module SGC.Utils (

  DoWith(..)

, RunDoWith
, doWith

) where

import Control.Applicative


newtype DoWith s a = DoWith (s -> a)

instance Functor (DoWith s) where
    fmap f (DoWith g) = DoWith $ f . g

instance Applicative (DoWith s) where
    pure = DoWith . const
    DoWith f <*> DoWith g = DoWith (\s -> f s $ g s)

instance Monad (DoWith s) where
    return = pure
    DoWith g >>= f = DoWith $ \s -> let DoWith f' = f (g s)
                                    in f' s


type RunDoWith s a = s -> DoWith s a -> a

doWith :: RunDoWith s a
doWith s (DoWith f) = f s
