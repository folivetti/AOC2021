module Day13 where 

import Data.List.Split (splitOn)
import Data.Set (Set) 
import qualified Data.Set as S 
import Data.List (foldl')

data Instruction = X Int | Y Int  deriving Show

getCoords :: [String] -> Set (Int, Int) 
getCoords = toSet . takeWhile (/="")

getInstructions :: [String] -> [Instruction] 
getInstructions = parseInstructions . tail . dropWhile (/="")

toSet :: [String] -> Set (Int, Int) 
toSet = S.fromList . map (readCoord . splitOn ",")

readCoord :: [String] -> (Int, Int)
readCoord [x,y] = (read x, read y)
readCoord _     = error "bad coordinates"

parseInstructions :: [String] -> [Instruction] 
parseInstructions [] = [] 
parseInstructions (x:xs) = case drop 11 x of 
                             ('x':'=':ys) -> X (read ys) : parseInstructions xs 
                             ('y':'=':ys) -> Y (read ys) : parseInstructions xs 
                             _            -> error "bad parser"

parseData :: [String] -> (Set (Int, Int), [Instruction])
parseData xss = (getCoords xss, getInstructions xss)

displayMap :: Set (Int, Int) -> String 
displayMap xs = unlines [ [ display (x,y) | x <- [0 .. maxOn fst xs] ] 
                                          | y <- [0 .. maxOn snd xs] 
                        ]
    where 
      maxOn f   = S.findMax . S.map f 
      display c = if c `S.member` xs then '#' else '.'

applyFold :: Set (Int, Int) -> Instruction -> Set (Int, Int) 
applyFold xs (X x) = S.map (\(a, b) -> if a >= x then (2*x - a, b) else (a, b)) xs
applyFold xs (Y y) = S.map (\(a, b) -> if b >= y then (a, 2*y - b) else (a, b)) xs
                         
part1 myMap = S.size . applyFold myMap . head 
part2 myMap = displayMap . foldl' applyFold myMap 

day13 :: IO ()
day13 = do 
    dat <- lines <$> readFile  "inputs/day13.txt"
    let myInput = parseData dat
    print $ uncurry part1 myInput
    putStrLn $ uncurry part2 myInput
