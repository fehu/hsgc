-----------------------------------------------------------------------------
--
-- Module      :  SGC2.Measures.SI
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


module SGC2.Measures.SI where

import SGC2.Measures.Definitions
import SGC2.Measures.PhysicalQuantities
import SGC2.Measures.Units

-----------------------------------------------------------------------------

data SI
instance UnitSystem SI where systemName _ = "SI"


--type instance UnitFor SI Distance = Meter


