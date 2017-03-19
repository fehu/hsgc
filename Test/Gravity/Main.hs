
{-# LANGUAGE FlexibleContexts #-}

import Graphics.UI.GLUT

import Test.Gravity.App.Gravity01

main = do (_progName, _args) <- getArgsAndInitialize
          _window <- createWindow "Gravity"
          windowSize $= Size 1000 1000
          (display, idle) <- app (5*10**(-10)) 0.02
          displayCallback $= display
          idleCallback $= idle
          mainLoop
