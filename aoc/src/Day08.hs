module Day08 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Reader

getInput :: String -> [([String],[String])]
getInput = map (toTuple . map words . splitOn " | ") . lines

toSets :: [([String],[String])] -> ([[S.Set Char]], [[S.Set Char]])
toSets xs = (map (map S.fromList . fst) xs, map (map S.fromList . snd) xs)

-- partial function but...meh! 
toTuple :: [a] -> (a, a)
toTuple [x,y] = (x,y)

getDigit p = head . filter p 
(.&&.) = liftM2 (&&) 

mkDict :: [S.Set Char] -> M.Map (S.Set Char) Int
mkDict digits = M.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]
  where
    -- part 1 
    one   = getDigit ((==2) . S.size) digits
    four  = getDigit ((==4) . S.size) digits
    seven = getDigit ((==3) . S.size) digits
    eight = getDigit ((==7) . S.size) digits
    -- 3 is the digit d such as d // 7 has size 2 and is different from 4 
    three = getDigit ((/=four) .&&. ((==2) . S.size . (`S.difference` seven))) digits
    -- 9 is the union of four and three 
    nine  = S.union four three
    -- zero is 8 // ((3 // 1) intersect 4)
    zero  = S.difference eight . S.intersection four . S.difference three $ one
    -- six is built by creating the letter E (8 // 1) and then finding the digit d that d intersection e = e 
    six   = let e = S.difference eight one in getDigit (((==e) . S.intersection e) .&&. (/=eight)) digits 
    -- five is the digit d that six intersect d == d 
    five  = getDigit ((\d -> S.intersection six d == d) .&&. (/=six)) digits
    -- two is the one left 
    two   = getDigit (\d -> d `notElem` [zero, one, three, four, five, six, seven, eight, nine]) digits

part1 :: [M.Map (S.Set Char) Int] -> [[S.Set Char]] -> Int
part1 maps digits = sum $ zipWith count maps digits
  where
    count m ds    = length $ filter (\d -> M.member d m && (m M.! d) `elem` [1,4,7,8]) ds

part2 :: [M.Map (S.Set Char) Int] -> [[S.Set Char]] -> Int
part2 maps digits = sum $ zipWith decode maps digits 
  where
      decode m ds = let (x:y:z:w:_) = map (m M.!) ds in x*1000 + y*100 + z*10 + w

day08 :: IO ()
day08 = do
    (inputs, displays) <- toSets . getInput <$> readFile "inputs/day08.txt"
    let maps = map mkDict inputs
    print $ part1 maps displays
    print $ part2 maps displays
