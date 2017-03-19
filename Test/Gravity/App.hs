
{-# LANGUAGE FlexibleContexts #-}

module Test.Gravity.App where

import Graphics.UI.GLUT hiding (Object)

import Test.Gravity         as Export
import Test.Gravity.Render  as Export
import Test.Environment2D   as Export

-----------------------------------------------------------------------------

app :: [Object] -> RenderObjects -> [Interaction] -> Float -> GLfloat
                -> IO (DisplayCallback, Maybe IdleCallback)
app objs d is t scale = do
  eVar <- newObjectEnv2DSysVar objs
  let disp = display eVar d is t scale
      idle = envUpdate eVar $ objsInteract is t
  return (disp, idle)


display eVar d is t scale' = do
  clear [ ColorBuffer ]
  loadIdentity
  scale scale' scale' scale'
  render d eVar
  flush

-----------------------------------------------------------------------------
