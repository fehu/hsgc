
module Test.Gravity  where

import Linear.V2
import Linear.Vector
import Linear.Metric

import Data.Function (on)

import Control.Arrow (first, second)

-----------------------------------------------------------------------------

type Mass = Float

type Position = V2 Float
type Speed    = V2 Float


data ObjectD = ObjectD { _objMass :: Mass
                       , _objId   :: String
                       }
  deriving (Eq, Ord, Show)

data ObjectP = ObjectP { _objPosition :: Position
                       , _objSpeed    :: Speed
                       }
  deriving (Eq, Ord, Show)

data Object = Object { _objD :: ObjectD, _objP :: ObjectP }
  deriving (Eq, Ord, Show)

mkObject :: String -> Mass -> Position -> Speed -> Object
mkObject name m p s = Object (ObjectD m name) (ObjectP p s)



objMass :: Object -> Float
objId   :: Object -> String
objPosition :: Object -> Position
objSpeed    :: Object -> Speed

objMass = _objMass . _objD
objId  = _objId . _objD
objPosition = _objPosition . _objP
objSpeed    = _objSpeed . _objP


objDistance :: Object -> Object -> Float
objDistance = distance `on` objPosition


-----------------------------------------------------------------------------

data Interaction = Interaction{
    interaction :: Object -> Object -> (V2 Float, V2 Float)
  }

-- interaction' :: (Interaction i) => i -> (Object, Object) -> (Object, Object)
-- interaction' i (x,y) = (x{_objP=px}, y{_objP=py})
--   where (px, py) = interaction i x y

-----------------------------------------------------------------------------

interact2 :: [Interaction] -> Object -> Object -> (V2 Float, V2 Float)
interact2 is x y = first sum . second sum
                 . unzip $ map (\i -> interaction i x y) is

applyInteractions :: Object -> [V2 Float] -> Float -> Object
applyInteractions x is t =
  let a = sum is ^/ objMass x
      speed'    = a ^* t
      speed     = objSpeed x + speed'
      position' = speed ^* t
      position  = objPosition x + position'
      objP = ObjectP position speed
  in x{ _objP = objP }


objsInteract :: [Interaction] -> Float -> [Object] -> [Object]
objsInteract is t xs = do x <- xs
                          let xi = do y <- xs
                                      if x /= y then return . fst
                                                     $ interact2 is x y
                                                else []
                          return $ applyInteractions x xi t


-- type InteractHook = [Object] -> IO ()

-- interactRun :: (Num time, Integral time) =>
--             [Interaction] -> [Object] -> time -> InteractHook -> IO ()
-- interactRun is xs t hook = let res = objsInteract is xs t
--                            in hook res >> interactRun is res t hook

-----------------------------------------------------------------------------

gravityConst :: Float
gravityConst = 6.67408*10^^(-11)

gravity = Interaction $
  \x y -> let f = gravityConst * objMass x* objMass y / objDistance x y ^^ 2
              dirVec a b = let q = (objPosition b - objPosition a)
                           in signorm q
          in (f *^ dirVec x y, f *^ dirVec y x)



-----------------------------------------------------------------------------

































-----------------------------------------------------------------------------






--
-- data ObjectD = ObjectD { _objMass :: Float
--                        , _objRad  :: Float
--                        }
--
-- data ObjectP = ObjectP { _objPosition :: Vec2 Float
--                        , _objSpeed    :: Vec2 Float
--                        }
--
-- data Object = Object { _objD :: ObjectD, _objP :: ObjectP }
--
-- objMass :: Object -> Float
-- objRad  :: Object -> Float
-- objPosition :: Object -> Vec2 Float
-- objSpeed    :: Object -> Vec2 Float
--
-- objMass = _objMass . _objD
-- objRad  = _objRad . _objD
-- objPosition = _objPosition . _objP
-- objSpeed    = _objSpeed . _objP
--
--
--
-- distance :: Object -> Object -> Float
-- distance a b = let (ax, ay) = objPosition a
--                    (bx, by) = objPosition b
--                in sqrt $ (ax-bx)^^2 + (ay-by)^^2
--
-- -----------------------------------------------------------------------------
--
-- class Interaction i where
--   interaction :: i -> Object -> Object -> (Vec2 Float, Vec2 Float)
--
-- -- interaction' :: (Interaction i) => i -> (Object, Object) -> (Object, Object)
-- -- interaction' i (x,y) = (x{_objP=px}, y{_objP=py})
-- --   where (px, py) = interaction i x y
--
-- -----------------------------------------------------------------------------
--
-- data Gravity = Gravity
--
-- gravityConst :: Float
-- gravityConst = 6.67408*10^^(-11)
--
-- instance Interaction Gravity where
--   interaction _ x y = undefined
--     where f = gravityConst * objMass x* objMass y / distance x y ^^ 2
--
--
