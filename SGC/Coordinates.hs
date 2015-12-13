{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  SGC.Coordinates
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module SGC.Coordinates (

  CoordinateSystem(..)
, InCoordinateSystem(..)

) where

import SGC.Measures

-----------------------------------------------------------------------------

class CoordinateSystem sys where

    type Coordinate sys :: * -> * -- Vector num


class InCoordinateSystem a sys usys num where
    coordinateOf :: ( PositionUnit usys ~ pos
                    , VectorUnit pos usys (Coordinate sys) num)
                 => sys -> a -> Maybe (pos num)





