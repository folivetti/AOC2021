{-# LANGUAGE TupleSections #-}
module Day15 where 

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as M 
import Data.Char ( digitToInt )
import Data.IntPSQ ( IntPSQ )
import qualified Data.IntPSQ as P 
import Data.List (foldl')
import Data.Maybe (mapMaybe) 

parseFile :: String -> Map (Int, Int) Int
parseFile = M.fromList . concat . zipWith parseLine [0..] . map (map digitToInt) . lines 

parseLine :: Int -> [Int] -> [((Int, Int), Int)]
parseLine ix = zipWith parseCoord [0..]
  where parseCoord iy x = ((ix,iy), x) 

getGoal :: Map (Int, Int) Int -> (Int, Int) 
getGoal = fst . M.findMax 

expand :: Map (Int, Int) Int -> Map (Int, Int) Int
expand cave = expanded cave
  where
    ((x, y),_)   = M.findMax cave
    shiftRight   = M.mapKeys (\(a,b) -> (a, b+y+1))
    shiftDown    = M.mapKeys (\(a,b) -> (a+x+1, b))
    fiveCopies f = take 5 . iterate (wrap . f)
    expanded     = M.unions . fiveCopies shiftDown . M.unions . fiveCopies shiftRight
    wrap         = fmap (\w -> if w == 9 then 1 else w + 1)

insertIfMin k c v q = case P.lookup k q of 
                        Nothing -> P.insert k c v q 
                        Just (c',_) -> if c' < c then q else P.insert k c v q 
{-# INLINE insertIfMin #-}

astar :: (Int, Int) -> IntPSQ Int Int -> Map (Int, Int) Int -> Int 
astar goal tq cv = go tq cv'
  where 
      cv'         = M.mapKeys toInt cv
      toInt (x,y) = x*width + y
      width       = snd goal + 1
      goal'       = toInt goal 

      go :: IntPSQ Int Int -> Map Int Int -> Int 
      go queue cave 
        | x == goal'     = cost
        | null neighbors = go q cave'
        | otherwise      = go queue' cave'
        where 
          Just (x, cost, _, q) = P.minView queue 
          queue'               = foldr (\(c,t) acc -> insertIfMin t c c acc) q neighbors 
          cave'                = M.delete x cave 

          neighbors = mapMaybe (\t -> (,t) . (+cost) <$> M.lookup t cave) 
                    $ inc x ++ [x+width, x-width]
          inc y 
            | y `rem` width == width - 1 = [y-1]
            | y `rem` width == 0         = [y+1]
            | otherwise                  = [y-1, y+1]

part1 cave = astar (getGoal cave) (P.singleton 0 0 0) cave
part2 cave = let cave' = expand cave 
             in  astar (getGoal cave') (P.singleton 0 0 0) cave'

day15 :: IO ()
day15 = do 
    dat <- parseFile <$> readFile "inputs/day15.txt"
    print (part1 dat)
    print (part2 dat)
