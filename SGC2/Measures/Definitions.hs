-----------------------------------------------------------------------------
--
-- Module      :  SGC2.Measures.Definitions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE FunctionalDependencies
           , TemplateHaskell
           , ExistentialQuantification
           , TypeOperators
           , FlexibleContexts
           , UndecidableInstances
       #-}

module SGC2.Measures.Definitions where


import Language.Haskell.TH

import TypeNum.Rational

-----------------------------------------------------------------------------

data Dimensions = Dimensionless | Scalar | Vector deriving (Show, Eq)

class PhysicalQuantity q where type QuantityDimensions q :: Dimensions
                               quantityName       :: q -> String


-- * Physical Quantities Code Generation
-----------------------------------------------------------------------------

genPhysicalQuantity :: String -> Dimensions -> Q [Dec] -- (PhysicalQuantity q) =>
genPhysicalQuantity name' dim = do
    let name = mkName name'
    dimType <- case dim of Dimensionless -> [t| Dimensionless |]
                           Scalar        -> [t| Scalar |]
                           Vector        -> [t| Vector |]
    sequence [ dataD (cxt []) name [] [] []
             , instanceD (cxt []) [t| PhysicalQuantity $(conT name)|]
                         [ return $ FunD (mkName "quantityName")
                                         [ Clause [WildP] (NormalB (LitE $ StringL name')) []]
                         , return $ TySynInstD (mkName "QuantityDimensions")
                                               (TySynEqn [ConT name] dimType)
                         ]
             ]



-----------------------------------------------------------------------------

class Unit u where unitName :: u -> String
                   unitInstance :: u

class UnitSystem sys where systemName :: sys -> String

type family UnitFor sys phq :: *

-----------------------------------------------------------------------------

data (Unit a, Unit b)       => a * b = (:*) a b
data (Unit a, Unit b)       => a / b = (:/) a b
data (Unit a, AsRational p) => a ^ p = (:^) a p

instance (Unit a, Unit b) => Unit (a * b) where
    unitName (a :* b) = unitName a ++ " * " ++ unitName b
    unitInstance = unitInstance :* unitInstance

instance (Unit a, Unit b) => Unit (a / b) where
    unitName (a :/ b) = unitName a ++ " / " ++ unitName b
    unitInstance = unitInstance :/ unitInstance

instance (Unit a, AsRational p, IntValuesForRational (AsRational' p)) => Unit (a ^ p) where
    unitName (a :^ p) = unitName a ++ "^" ++ show (asRational p)
    unitInstance = unitInstance :^ asRational'

-----------------------------------------------------------------------------

class Measured m u v | m -> u, m -> v
    where type Prefix m
          measuredUnit     :: m -> u
          measuredPrefix   :: m -> Maybe (Prefix m)
          measuredRawValue :: m -> v

          measuredValue :: m -> v
          measured  :: v -> Prefix m -> u -> m
          measured' :: v -> u -> m

-----------------------------------------------------------------------------
