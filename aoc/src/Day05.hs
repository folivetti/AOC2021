{-# LANGUAGE TupleSections #-}
module Day05 where 

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Point = (Int, Int) 
data Line = Line Point Point deriving Show 

parseLine :: String -> Line 
parseLine xs = let [x,y] = splitOn " -> " xs 
               in  Line (parsePoint x) (parsePoint y) 

parsePoint :: String -> Point 
parsePoint xs = let [x,y] = splitOn "," xs 
                in  (read x, read y) 

isHor, isVer  :: Line -> Bool 
isHor (Line (x,y) (a,b)) = x == a
isVer (Line (x,y) (a,b)) = y == b 

isStraight :: Line -> Bool
isStraight x = isHor x || isVer x  

range :: Int -> Int -> [Int] 
range x y = [x, op x .. y] 
    where op = if x <= y then (+1) else subtract 1

makeLine :: Line -> [Point] 
makeLine (Line (x, y) (a, b)) 
  | x == a    = [(x, z) | z <- range y b]
  | y == b    = [(z, y) | z <- range x a]
  | otherwise = zip (range x a) (range y b)

makeCounter :: [Point] -> M.Map Point Int 
makeCounter = M.fromListWith (+) . map (,1)

twoOrMore :: M.Map Point Int -> [(Point, Int)]
twoOrMore = M.toList . M.filter (>1) 

countIntersectionPoints :: [Line] -> Int 
countIntersectionPoints = length 
                        . twoOrMore 
                        . makeCounter 
                        . concatMap makeLine 

part1 :: [Line] -> Int
part1 = countIntersectionPoints . filter isStraight 

part2 :: [Line] -> Int 
part2 = countIntersectionPoints

day05 :: IO ()
day05 = do
    dat <- map parseLine . lines <$> readFile "inputs/day05.txt"
    print $ part1 dat
    print $ part2 dat
