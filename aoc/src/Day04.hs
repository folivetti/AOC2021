module Day04 where

import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M

type Bingo = [Either Int Int]

readDraws :: String -> [Int]
readDraws = map read . splitOn ","

readCharts :: [String] -> [Bingo]
readCharts css = map readChart charts
  where
    charts    = splitOn [""] css
    readChart = concatMap (map (Left . read) . words)

changeCharts :: Int -> [Bingo] -> [Bingo]
changeCharts x = map (changeChart x)

changeChart :: Int -> Bingo -> Bingo
changeChart x = map (change x)
    where
      change y (Left z) | y == z = Right z
      change _ z                 = z

hasWon :: Bingo -> Bool
hasWon bingo = checkWin (`quot` 5) markedIxs || checkWin (`rem` 5) markedIxs
  where
    markedIxs   = getMarkedIxs bingo
    toKeyWith f = map (\x -> (f x, 1))
    checkWin f  = any (\(k,v) -> v==5) . M.toList . M.fromListWith (+) . toKeyWith f
    getMarkedIxs = map fst . filter (isRight . snd) . zip [0..]

sequenceOfWinners :: [Bingo] -> [Int] -> [(Int, [Bingo])]
sequenceOfWinners bingos [] = []
sequenceOfWinners []     _  = []
sequenceOfWinners bingos (x:xs)
  | null winners = sequenceOfWinners losers xs
  | otherwise    = (x, winners) : sequenceOfWinners losers xs
  where
    b'          = changeCharts x bingos
    winners     = filter hasWon b'
    losers      = filter (not.hasWon) b'

calculateScores :: Int -> [Bingo] -> [Int]
calculateScores x = map ((x*) . sumOfUnmarked)


isRight, isLeft :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
isLeft (Left _)   = True
isLeft _          = False

sumOfUnmarked :: Bingo -> Int
sumOfUnmarked []             = 0
sumOfUnmarked (Left x : xs)  = x + sumOfUnmarked xs
sumOfUnmarked (Right x : xs) = sumOfUnmarked xs

day04 :: IO ()
day04 = do
    (drawsStr:chartsStr) <- lines <$> readFile "inputs/day04.txt"
    let draws   = readDraws drawsStr
        charts  = readCharts chartsStr
        winners = sequenceOfWinners charts draws
        scores  = map (uncurry calculateScores) winners
    print $ head scores
    print $ last scores
