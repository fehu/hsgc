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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SGC.Object.Generic.Create where

import SGC.Object.Generic
import SGC.Object.Generic.TypeMap

-----------------------------------------------------------------------------

instance (TypeMap cs, TypeMap vs) =>
  CreateSomeObject (GenericObject base cs vs) base where
    someObject obj = SomeObject obj objBase

-----------------------------------------------------------------------------
