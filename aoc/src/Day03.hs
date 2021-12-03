module Day03 where

import Data.Foldable
import Control.Arrow ( Arrow((&&&)) )
import Data.List

data Counter = Counter { bit0 :: Int, bit1 :: Int } deriving Show

instance Semigroup Counter where
    Counter x y <> Counter a b = Counter (x+a) (y+b)

instance Monoid Counter where
    mempty = counter

counter :: Counter
counter = Counter 0 0

parseChar :: Char -> Counter
parseChar '0' = Counter 1 0
parseChar '1' = Counter 0 1
parseChar _   = counter

toCounter :: [String] -> [[Counter]]
toCounter = map (map parseChar)

countData :: [[Counter]] -> [Counter]
countData = foldr (zipWith (<>)) (repeat counter)

str2data :: [String] -> [Counter]
str2data = countData . toCounter

getBit :: Counter -> Int
getBit = mostCommon

mostCommon :: Counter -> Int
mostCommon (Counter z o)
  | z > o     = 0
  | otherwise = 1

leastCommon :: Counter -> Int
leastCommon (Counter z o)
  | z > o     = 1
  | otherwise = 0

toGamma :: [Counter] -> [Int]
toGamma = map mostCommon

toEps :: [Counter] -> [Int]
toEps = map leastCommon

bin2dec :: [Int] -> Int
bin2dec = foldl (\a -> (+) (2*a)) 0

gammaVal = bin2dec . toGamma
epsVal   = bin2dec . toEps

part1 :: [String] -> Int
part1 = uncurry (*) . (gammaVal &&& epsVal) . str2data

part2 :: [String] -> Int
part2 xs = getOxygen cs * getCO2 cs
  where
    cs = toCounter xs

getOxygen :: [[Counter]] -> Int
getOxygen = bin2dec . findValue mostCommon
getCO2 :: [[Counter]] -> Int
getCO2    = bin2dec . findValue leastCommon

findValue :: (Counter -> Int) -> [[Counter]] -> [Int]
findValue f []  = []
findValue f [x] = map getBit x
findValue f xs  = bit : findValue f xs'
    where
      mSplit = mapM uncons xs :: Maybe [(Counter, [Counter])]
      bit    = maybe 0 (f . foldMap fst) mSplit
      xs'    = maybe [] (map snd . filter ((==bit).getBit.fst)) mSplit


day03 :: IO ()
day03 = do
    dat <- lines <$> readFile "inputs/day03.txt"
    print $ part1 dat
    print $ part2 dat
