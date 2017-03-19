
module Test.Gravity.Render  where

import Graphics.UI.GLUT hiding (Object)

import Test.Environment2D
import Test.Gravity

import Linear.V2

import Data.IORef

-----------------------------------------------------------------------------

type Render = IO ()

data RenderObjects = RenderObjects (String -> Render)

instance Renderable RenderObjects Object where
  render (RenderObjects render') = render' . objId

type ObjectEnv2DSysVar = Env2DSysVar Object Float

newObjectEnv2DSysVar :: [Object] -> IO ObjectEnv2DSysVar
newObjectEnv2DSysVar = newIORef . flip Env2DSys (v2Tuple . objPosition)

v2Tuple (V2 x y) = (x,y)
