module Day17 where

import Data.Maybe (mapMaybe) 

type Coord = (Int, Int)

data Projectile = Proj { _x :: Int
                       , _y :: Int
                       , _x_speed :: Int
                       , _y_speed :: Int
                       } deriving Show

example, myInput :: (Coord, Coord)
example = ((20, 30), (-10, -5))
myInput = ((85, 145), (-163, -108))

step :: Projectile -> Projectile
step (Proj x y x_sp y_sp) = Proj (x + x_sp) (y + y_sp) (x_sp - signum x_sp) (y_sp - 1)
{-# INLINE  step #-}

isNotOver :: (Coord, Coord) -> Projectile -> Bool
isNotOver ((minX, maxX), (minY, maxY)) (Proj x y _ _) = x <= maxX && y >= minY
{-# INLINE  isNotOver #-}

isWithin :: (Coord, Coord) -> Projectile -> Bool
isWithin ((minX, maxX), (minY, maxY)) (Proj x y _ _) = x >= minX && y <= maxY 
{-# INLINE  isWithin #-}

-- | returns the maximum height only if it hits the target 
-- else, it returns Nothing 
getMaximumHeight :: (Coord, Coord) -> Projectile -> Maybe Int 
getMaximumHeight bounds = toMaybe . takeWhile (isNotOver bounds) . iterate step 
  where 
      toMaybe ps | isWithin bounds (last ps) = Just $ maximum $ map _y ps 
                 | otherwise                 = Nothing 
{-# INLINE  getMaximumHeight #-}

findBest bounds@((_, maxX), (minY, _)) = mapMaybe (getMaximumHeight bounds . uncurry (Proj 0 0)) coords 
        where coords = (,) <$> [0..maxX] <*> [minY .. negate minY]

day17 :: IO ()
day17 = do
    let feasibles = findBest myInput
    print $ maximum feasibles 
    print $ length feasibles 
