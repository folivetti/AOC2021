{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import Data.List ( sort, foldl' )
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8

example :: [Int]
example = [16,1,2,0,4,2,7,1,2,14]

mean :: [Int] -> Int
mean xs = round $ fromIntegral acc / fromIntegral count 
  where
    (acc, count) = foldl' (\(s, c) x -> (s + x, c + 1)) (0,0) xs 

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
sumOfDiffsPA n = foldl' (\acc x -> acc + f x) 0
  where
      f x = let y = abs (x - n) in y * (y + 1) `div` 2 

part1 :: [Int] -> Int
part1 xs = sumOfDiffs (median xs) xs

part2 :: [Int] -> Int
part2 xs = minimum [sumOfDiffsPA (mx-1) xs, sumOfDiffsPA mx xs, sumOfDiffsPA (mx+1) xs]
  where
    mx = mean xs

parser1 :: Parser [Int]
parser1 = decimal `sepBy` char ','
{-# INLINE parser1 #-}

fromResult :: Result [Int] -> [Int]
fromResult (Done _ xs) = xs
fromResult _ = []
{-# INLINE fromResult #-}

day07 :: IO ()
day07 = do
    dat <- B.readFile  "inputs/day07.txt"
    let myInput = fromResult $ parse parser1 dat
    print $ part1 myInput
    print $ part2 myInput