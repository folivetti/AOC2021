{-# LANGUAGE TupleSections #-}
module Day11 where

import Data.Map.Strict ( Map, (!), (!?) )
import qualified Data.Map.Strict as M
import Data.Char ( digitToInt, intToDigit )
import Control.Monad.State
import Data.Maybe (mapMaybe)

type Octopuses = Map (Int, Int) Int
type Energy a = State Octopuses a

parseMtx :: [String] -> Octopuses
parseMtx = M.unions . map (uncurry parseRow) . enumerate

parseRow :: Int -> String -> Octopuses
parseRow ix = M.fromList . map (\(iy,c) -> ((ix,iy), digitToInt c)) . enumerate

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

deltas :: [(Int, Int)]
deltas = filter (/=(0,0)) $ (,) <$> [-1..1] <*> [-1..1]

(.+.) :: Num a => (a, a) -> (a, a) -> (a, a)
(x,y) .+. (a,b) = (x+a, y+b)


countN :: Eq a => a -> Map (Int, Int) a -> (Int, Int) -> Int
countN n s idx = length . filter (==n) . mapMaybe ((s !?) . (idx .+.)) $ deltas 

repeatWhileNEQ :: Eq a => (a -> a) -> a -> a 
repeatWhileNEQ f x 
  | y == x    = x
  | otherwise = repeatWhileNEQ f y 
  where y = f x 

printGrid :: Octopuses -> IO ()
printGrid m = mapM_ printRow [0..9] >> putChar '\n'
    where
        printRow ix = mapM_ (putChar . intToDigit . (m !) . (ix,)) [0..9] >> putChar '\n'

updateMap :: Map (Int, Int) Int -> Map (Int, Int) (Either Int Int) 
updateMap m = M.mapWithKey upd m 
    where 
        upd k x | x == 9    = Left 0 
                | otherwise = Right $ x + nines k + 1
        nines idx = length . filter (==9) . mapMaybe ((m !?) . (idx .+.)) $ deltas

updateMapEither :: Map (Int, Int) (Either Int Int) -> Map (Int, Int) (Either Int Int) 
updateMapEither m = M.mapWithKey upd m 
    where 
        upd k (Left x) = Left x 
        upd k (Right x) | x > 9     = Left 0 
                        | otherwise = Right $ x + nines k 
        nines idx = length . filter isGT9 . mapMaybe ((m !?) . (idx .+.)) $ deltas
        isGT9 (Left x) = False 
        isGT9 (Right x) = x > 9

flash :: Map (Int, Int) (Either Int Int) -> Map (Int, Int) Int 
flash = M.map fromFlash . repeatWhileNEQ updateMapEither
  where 
      fromFlash (Left x)  = x 
      fromFlash (Right x) = x 

step :: (Octopuses -> a) -> Energy a
step f = state $ \s ->
    let s' = flash $ updateMap s
    in (f s', s')

countFlashes :: Octopuses -> Int 
countFlashes = M.size . M.filter (==0) 

runSteps :: Int -> Energy Int 
runSteps 0 = pure 0 
runSteps n = do x <- step countFlashes 
                y <- runSteps (n-1)
                pure (x+y)

runUntilAllEq :: Int -> Energy Int 
runUntilAllEq n = do 
    x <- step countFlashes 
    if x == 100
       then pure $ n + 1
       else runUntilAllEq (n+1) 

debugSteps :: Int -> Energy Octopuses 
debugSteps 0 = get 
debugSteps n = do step id 
                  debugSteps (n-1)


day11 :: IO ()
day11 = do
  dat <- lines <$> readFile "inputs/day11.txt"
  let mtx = parseMtx dat
  printGrid mtx
  let mtx' = debugSteps 1 `evalState` mtx
  printGrid mtx'
  let x = runSteps 100 `evalState` mtx
  print x
  let x = runUntilAllEq 0 `evalState` mtx 
  print x
