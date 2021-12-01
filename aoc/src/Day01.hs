module Day01
    ( day01
    ) where

import Control.Comonad ( Comonad(extend) ) 
import qualified Data.List.NonEmpty as NE 
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ( Sum(Sum, getSum) ) 

slidingSums :: Int -> NonEmpty Int -> NonEmpty Int 
slidingSums n = extend (sum . NE.take n) 

isIncreasing :: NonEmpty Int -> Bool
isIncreasing (x :| (y: _)) = y > x
isIncreasing _             = False

countIncreasing :: NonEmpty Int -> Int 
countIncreasing = getSum . foldMap (Sum . fromEnum) . extend isIncreasing

partA :: NonEmpty Int -> IO ()
partA = print . countIncreasing

partB :: NonEmpty Int -> IO ()
partB = print . countIncreasing . slidingSums 3

day01 :: IO ()
day01 = do
    dat <- NE.fromList . map read . lines <$> readFile "inputs/day01.txt"
    partA dat 
    partB dat
