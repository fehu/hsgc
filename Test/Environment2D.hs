
{-# LANGUAGE FlexibleInstances #-}

module Test.Environment2D where

import Graphics.UI.GLUT

import Data.IORef

import Control.Arrow ( (&&&) )
import Control.Exception ( SomeException )

-----------------------------------------------------------------------------

type Coord2D a = (a, a)

data Env2DSys obj a = Env2DSys [obj] (obj -> Coord2D a)

instance (Show obj) => Show (Env2DSys obj a) where
  show (Env2DSys xs _) = "Env2DSys " ++ show xs

class Renderable d a where render :: d -> a -> IO ()

instance (Renderable d obj, Num a, MatrixComponent a) =>
  Renderable d (Env2DSys obj a) where
    render d (Env2DSys l cf) = sequence_ $ do
      (o, (x, y)) <- (id &&& cf) <$> l
      return . preservingMatrix $ translate (Vector3 x y 0) >> render d o


type Env2DSysVar obj a = IORef (Env2DSys obj a)

instance (Renderable d obj, Num a, MatrixComponent a) =>
  Renderable d (Env2DSysVar obj a) where
    render d var = render d =<< readIORef var
                      -- putStrLn "render"



envUpdate :: (Show obj) => Env2DSysVar obj a -> ([obj] -> [obj]) -> Maybe IdleCallback
envUpdate var update = Just $ do Env2DSys xs cf <- readIORef var
                                 let xs' = update xs
                                 var `writeIORef` Env2DSys xs' cf
                                --  putStrLn "EnvUpdate"
                                --  print xs'
                                 postRedisplay Nothing



-- envRun :: (Renderable d obj, Num a, MatrixComponent a, Show obj) =>
--           Env2DSysVar obj a -> d -> EnvUpdateHook obj -> IO ()
-- envRun envVar d update = do env@(Env2DSys xs cf) <- readIORef envVar
--                             let xs'  = update xs
--                                 env' = Env2DSys xs' cf
--                             envVar `writeIORef` env'
--                             putStrLn $ "Env updated: " ++ show env'
--                             render d env'




-- data EnvExec = EnvExec { envStop :: IO ()
--                        , envAwait :: IO (Maybe SomeException)
--                        }

-- envRun :: Env2DSys obj a -> EnvUpdateHook obj -> IO ()
-- envRun env@(Env2DSys xs _) =


-- interactRun :: (Num time, Integral time) =>
--             [Interaction] -> [Object] -> time -> InteractHook -> IO ()
-- interactRun is xs t hook = let res = objsInteract is xs t
--                            in hook res >> interactRun is res t hook


-- vec3 :: Float -> Float -> Float -> Vector3 Float
-- vec3 = Vector3

-- vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
--
-- renderSmth = renderPrimitive Quads $ vertex3f 0 0 0
--                                   >> vertex3f 1 0 0
--                                   >> vertex3f 1 1 0
--                                   >> vertex3f 0 1 0
--
--
-- test = do renderSmth
--           preservingMatrix $ do translate $ vec3 3 0 0
--                                 renderSmth
