module Test.Gravity.Orbit where


import Test.Gravity

import Linear.Metric
import Linear.Matrix
import Linear.Vector
import Linear.V2


-----------------------------------------------------------------------------

-- * r*v^2 == G*M

orbitByRadius :: Mass -> Position -> Speed
orbitByRadius m x = let v = sqrt (gravityConst * m / norm x)
                        rotate = V2 (V2 0 (-1))
                                    (V2 1   0)
                    in v *^ (rotate !* signorm x) -- perp (signum x)

orbitByPeriod :: Mass -> Float -> Float
orbitByPeriod m t = (gravityConst*m*t^2/(4*pi^2)) ** 0.2


-----------------------------------------------------------------------------
