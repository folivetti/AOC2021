module Day01
    ( day01
    ) where

import Control.Comonad 
import qualified Data.List.NonEmpty as NE 
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid 

slidingSums :: Int -> NonEmpty Int -> NonEmpty Int 
slidingSums n = extend (sum . NE.take n) 

isIncreasing :: NonEmpty Int -> Bool
isIncreasing (x :| (y: _)) = y > x
isIncreasing _             = False

countIncreasing :: NonEmpty Int -> Int 
countIncreasing = getSum . foldMap (Sum . b2i) . extend isIncreasing

b2i :: Bool -> Int 
b2i True  = 1 
b2i False = 0

partA = print . countIncreasing
partB = print . countIncreasing . slidingSums 3

day01 :: IO ()
day01 = do
    dat <- NE.fromList . map read . lines <$> readFile "inputs/day01.txt"
    partA dat 
    partB dat
