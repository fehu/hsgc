-----------------------------------------------------------------------------
--
-- Module      :  SGC.Object.Measures.Templates
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TemplateHaskell #-}

module SGC.Object.Measures.Templates (

  _genMeasurableObjKey

) where

import Language.Haskell.TH

import PhyQ
import SGC.Object

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

_genMeasurableObjKey :: (PhysicalQuantity q) => String -> q -> DecsQ
_genMeasurableObjKey name' q = do
  let obj' = mkName "obj"
      t'   = mkName "t"
      m'   = mkName "m"
      v'   = mkName "v"
      qt'  = mkName $ quantityName q
      name = mkName $ "Obj" ++ name'
      has' = mkName $ "Has" ++ name'
      readF'      = mkName $ "read" ++ name'
      getF'       = mkName $ "get" ++ name'
      writeF'     = mkName $ "write" ++ name'
      updateF'    = mkName $ "update" ++ name'
      updateValF' = mkName $ "update" ++ name' ++ "Value"

      obj = varT obj'
      t   = varT t'
      m   = varT m'
      v   = varT v'
      qt  = conT qt'
      k   = conT name
      has = conT has'

      kExpr = conE name

  data' <- dataD (cxt []) name [PlainTV v'] Nothing
                               [normalC name []] (cxt [])

  instK <- instanceD (cxt [ [t|Typeable $v|] ])
                     [t|ObjectKey ($k $v)|]
                     [
                       tySynInstD (mkName "KeyValue")
                                $ tySynEqn [[t|$k $v|]]
                                           [t|Measurable $qt $v|]
                     ]

  hasConstr <- tySynD (mkName $ "Has" ++ name')
               [PlainTV obj', PlainTV t', PlainTV m', PlainTV v']
               [t|ObjectHas $obj $m $t '[$k $v]|]

  let fConstr c = forallT [PlainTV obj', PlainTV m', PlainTV v']
                          (cxt [ [t|$has $obj $c $m $v|] ])
      fClauses e = [clause [] (normalB e) []]

      mkF fname c sig e = sequence [ sigD fname $ fConstr c sig
                                   , funD fname $ fClauses e ]

  readFunc <- mkF readF' [t|Any|] [t|$obj -> $m (Measurable $qt $v)|]
                         [e|readValue $kExpr|]

  getFunc <- mkF getF' [t|Const|] [t|$obj -> Measurable $qt $v|]
                       [e|getConst $kExpr|]

  writeFunc <- mkF writeF' [t|Var|] [t|Measurable $qt $v -> $obj -> $m ()|]
                           [e|writeVar $kExpr|]

  updateFunc <- mkF updateF' [t|Var|]
                             [t|(Measurable $qt $v -> Measurable $qt $v) -> $obj -> $m ()|]
                             [e|updateVar $kExpr|]

  updateValFunc <- mkF updateValF' [t|Var|] [t|($v -> $v) -> $obj -> $m ()|]
                                   [e|updateVar $kExpr . fmap|]

  return $ [data', instK, hasConstr]
        ++ readFunc ++ getFunc ++ writeFunc ++ updateFunc ++ updateValFunc

-----------------------------------------------------------------------------
