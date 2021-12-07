module Day07 where 

import Data.List ( sort )
import Data.List.Split ( splitOn )
import Control.Arrow ( (&&&) )

example :: [Int] 
example = [16,1,2,0,4,2,7,1,2,14]

mean :: [Int] -> Int 
mean xs = round $ sum (map fromIntegral xs) / fromIntegral (length xs)  

median :: [Int] -> Int 
median xs 
  | even n     = avg 
  | otherwise  = midpoint 
  where 
    xs' = sort xs 
    n   = length xs
    ix  = n `div` 2 
    avg = ((xs' !! ix) + (xs' !! (ix-1))) `div` 2
    midpoint = xs' !! ix 

sumOfDiffs :: Int -> [Int] -> Int 
sumOfDiffs n xs = sum $ map (abs . subtract n) xs 

sumOfDiffsPA :: Int -> [Int] -> Int 
sumOfDiffsPA n xs = sum $ map f xs 
  where 
      f x = sum [0 .. abs (x - n)]

part1 :: [Int] -> Int 
part1 xs = sumOfDiffs (median xs) xs 

part2 :: [Int] -> Int 
part2 xs = minimum $ map (`sumOfDiffsPA` xs) [mx - 1, mx, mx + 1] 
  where 
    mx = mean xs 

day07 :: IO ()
day07 = do
    dat <- map read . splitOn "," <$> readFile  "inputs/day07.txt"
    print $ part1 example
    print $ part1 dat
    print $ part2 example
    print $ part2 dat
