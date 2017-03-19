module Test.Gravity.App.Gravity01 where


import Test.Gravity
import Test.Gravity.Render
import Test.Gravity.Orbit

import Linear.V2

import Graphics.UI.GLUT hiding (Object)

import qualified Test.Gravity.App as App

import Control.Monad (forM_)

-----------------------------------------------------------------------------

starMass = 10^23
star = mkObject "Star" starMass (V2 0 0) (V2 0 0)


planet1Radius    = 2
planet1InitPos   = V2 0 planet1Radius
planet1InitSpeed = orbitByRadius starMass planet1InitPos
planet1 = mkObject "Planet 1" (10^16) planet1InitPos planet1InitSpeed

-- planetPeriod = 5*10^5
-- planetRadius = orbitByPeriod starMass planetPeriod

planet2Radius    = 4
planet2InitPos   = V2 0 planet2Radius
planet2InitSpeed = orbitByRadius starMass planet2InitPos + V2 0 500000
planet2 = mkObject "Planet 2" (10^16) planet2InitPos planet2InitSpeed

planet3Radius    = 6
planet3InitPos   = V2 0 planet3Radius
planet3InitSpeed = orbitByRadius starMass planet3InitPos
planet3 = mkObject "Planet 3" (10^16) planet3InitPos planet3InitSpeed


planet4Radius    = 8
planet4InitPos   = V2 0 (-planet4Radius)
planet4InitSpeed = orbitByRadius starMass planet4InitPos
planet4 = mkObject "Planet 4" (10^14) planet4InitPos planet4InitSpeed

planet5Radius    = 14
planet5InitPos   = V2 planet5Radius 0
planet5InitSpeed = orbitByRadius starMass planet5InitPos
planet5 = mkObject "Planet 5" (10^21) planet5InitPos planet5InitSpeed

planet6Radius    = 18
planet6InitPos   = V2 planet6Radius 0
planet6InitSpeed = orbitByRadius starMass planet6InitPos
planet6 = mkObject "Planet 6" (10^20) planet6InitPos planet6InitSpeed

renderObjs = RenderObjects $ \name ->
          case name of "Star" -> renderCircle 0.2
                       _      -> renderCircle 0.1

renderCircleQuality = 12
renderCircle r =
  let a' = 2 * pi / fromIntegral renderCircleQuality :: Float
      as = take renderCircleQuality $ iterate (a' +) 0
  in renderPrimitive LineLoop . forM_ as $
        \a -> vertex (Vertex2 (r*cos a) (r*sin a))


app = App.app [star, planet1, planet2, planet3, planet4, planet5, planet6]
              renderObjs [gravity]

-----------------------------------------------------------------------------
