{-# LANGUAGE TupleSections #-}
module Day11 where

import Data.Map.Strict ( Map, (!), (!?) )
import qualified Data.Map.Strict as M
import Data.Char ( digitToInt, intToDigit )
import Control.Monad.State
import Data.Maybe (mapMaybe)

data Octopus   = Energy Int | Flashed deriving Eq 
type Octopuses = Map (Int, Int) Octopus
type Energy a  = State Octopuses a

instance Show Octopus where
    show (Energy x) = show x
    show Flashed    = "0"

parseMtx :: [String] -> Octopuses
parseMtx = M.unions . map (uncurry parseRow) . enumerate

parseRow :: Int -> String -> Octopuses
parseRow ix = M.fromList . map (\(iy,c) -> ((ix,iy), Energy $ digitToInt c)) . enumerate

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

deltas :: [(Int, Int)]
deltas = filter (/=(0,0)) $ (,) <$> [-1..1] <*> [-1..1]

(.+.) :: Num a => (a, a) -> (a, a) -> (a, a)
(x,y) .+. (a,b) = (x+a, y+b)

printGrid :: Octopuses -> IO ()
printGrid m = mapM_ printRow [0..9] >> putChar '\n'
    where
        printRow ix = mapM_ (putStr . show . (m !) . (ix,)) [0..9] >> putChar '\n'

inc :: Octopus -> Octopus
inc (Energy x) = Energy $ x + 1
inc Flashed    = Flashed

unflash :: Octopus -> Octopus
unflash Flashed = Energy 0
unflash x       = x

isFlashing :: Octopus -> Bool
isFlashing (Energy x) = x > 9
isFlashing _          = False

updateMap :: Octopuses -> Octopuses
updateMap = M.map unflash . flash . M.map inc

flash :: Octopuses -> Octopuses
flash m | m == m'   = m
        | otherwise = flash m'
  where
    m' = M.mapWithKey upd m
    upd k (Energy x)
      | x > 9     = Flashed
      | otherwise = Energy $ x + flashing k
    upd k Flashed = Flashed
    flashing k    = length . filter isFlashing . mapMaybe ((m !?) . (k .+.)) $ deltas

step :: Energy Int 
step = do modify updateMap
          gets countFlashes

countFlashes :: Octopuses -> Int
countFlashes = M.size . M.filter (== Energy 0)

repeatUntilM :: (Int -> Bool) -> Energy [Int]
repeatUntilM p = do x <- step 
                    if p x 
                       then pure [x]
                       else (x:) <$> repeatUntilM p 

debugGrid :: Energy a -> Octopuses -> IO () 
debugGrid e = printGrid . execState e

part1 :: Energy Int 
part1 = sum <$> replicateM 100 step 

part2 :: Energy Int 
part2 = length <$> repeatUntilM (==100)

day11 :: IO ()
day11 = do
  mtx <- parseMtx . lines <$> readFile "inputs/day11.txt"
  print $ part1 `evalState` mtx
  print $ part2 `evalState` mtx
  debugGrid part1 mtx
