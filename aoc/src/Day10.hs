{-# LANGUAGE DeriveFunctor #-}
module Day10 where 

import Data.List (sort, foldl')

data Status a = Valid | Invalid a | Incomplete String deriving (Show, Functor)

opening :: [Char] 
opening = "[({<" 
closing :: [Char] 
closing = "])}>"

inverse :: Char -> Char 
inverse ')' = '('
inverse '(' = ')' 
inverse '{' = '}' 
inverse '}' = '{' 
inverse '[' = ']' 
inverse ']' = '[' 
inverse '<' = '>' 
inverse '>' = '<' 
inverse c   = c

isInverseOf :: Char -> Char -> Bool
isInverseOf c1 c2 = inverse c1 == c2

checkStatus :: [Char] -> Status Char 
checkStatus = go [] 
    where 
        go [] []    = Valid 
        go stack [] = Incomplete stack
        go []    (c:cs) 
          | c `elem` opening = go [c] cs 
          | otherwise        = Invalid c 
        go (s:ss) (c:cs) 
          | c `elem` opening                      = go (c:s:ss) cs 
          | c `elem` closing && c `isInverseOf` s = go ss cs 
          | otherwise                             = Invalid c

char2points :: Char -> Int 
char2points ')' = 3 
char2points ']' = 57 
char2points '}' = 1197 
char2points '>' = 25137 
char2points _   = 0

char2points2 :: Char -> Int 
char2points2 ')' = 1
char2points2 ']' = 2 
char2points2 '}' = 3 
char2points2 '>' = 4 
char2points2 _   = 0

sumInvalids :: [Status Int] -> Int 
sumInvalids [] = 0 
sumInvalids (Invalid x : xs) = x + sumInvalids xs 
sumInvalids (_:xs) = sumInvalids xs 

isIncomplete :: Status a -> Bool 
isIncomplete (Incomplete _) = True 
isIncomplete _ = False 

getStack :: Status a -> String 
getStack (Incomplete stack) = stack 
getStack _ = [] 

incompleteScore :: [Char] -> Int
incompleteScore = foldl' (\acc c -> 5*acc + char2points2 c) 0
{-# INLINE incompleteScore #-}

middleScore :: [Int] -> Int 
middleScore xs = sort xs !! (n `div` 2)
  where 
      n = length xs 

part1, part2 :: [Status Char] -> Int
part1 = sumInvalids . map (fmap char2points)
part2 = middleScore . map (incompleteScore . map inverse . getStack) . filter isIncomplete 

day10 :: IO ()
day10 = do 
    dat <- map checkStatus . lines <$> readFile "inputs/day10.txt"
    print $ part1 dat
    print $ part2 dat
