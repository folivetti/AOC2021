{-# LANGUAGE OverloadedStrings #-}

module Day18 where 

import Data.Char (digitToInt, isDigit)
import qualified Data.Attoparsec.ByteString.Char8 as P 
import qualified Data.ByteString.Char8 as B 
import Control.Applicative ( Alternative((<|>)) ) 
import Data.List (foldl1')
import Data.Bifunctor ( Bifunctor(first) ) 

data Tree = Leaf Int | Branch Tree Tree deriving (Show, Eq)

parseTree :: P.Parser Tree 
parseTree =  P.try (Leaf <$> P.decimal)
         <|> Branch <$> leftParse <*> rightParse

leftParse :: P.Parser Tree 
leftParse = do P.char '[' 
               t <- parseTree 
               P.char ',' 
               pure t

rightParse :: P.Parser Tree 
rightParse = do t <- parseTree 
                P.char ']' 
                pure t

parse' :: B.ByteString -> Tree
parse' b = case P.parse parseTree b of 
             P.Done _ r -> r 
             _        -> error "parser error" 

add :: Tree -> Tree -> Tree
add = Branch

explode :: Tree -> Tree
explode = pickTree . go 0
  where
    pickTree (t, _, _) = t 

    addRight 0 t            = t
    addRight x (Leaf y)     = Leaf (x+y)
    addRight x (Branch l r) = Branch l (addRight x r)

    addLeft 0 t            = t
    addLeft x (Leaf y)     = Leaf (x+y)
    addLeft x (Branch l r) = Branch (addLeft x l) r

    go d (Leaf x)                     = (Leaf x, 0, 0)
    go d t@(Branch (Leaf x) (Leaf y)) = if d >= 4 then (Leaf 0, x, y) else (t, 0, 0)
    go d (Branch l r)                 = (Branch (addRight z l') r', x, w)
      where 
        (l', x, y) = go (d + 1) l 
        (r', z, w) = go (d + 1) (addLeft y r)

split :: Tree -> (Tree, Bool) 
split (Branch l r) = case split l of 
                       (l', True)  -> (Branch l' r, True)
                       (l', False) -> first (Branch l') (split r) 
split (Leaf x) 
  | x >= 10   = (Branch (Leaf q) (Leaf (q + r)), True) 
  | otherwise = (Leaf x, False)
  where 
      (q, r) = x `quotRem` 2 

depth (Branch l r) = 1 + max (depth l) (depth r) 
depth (Leaf _)     = 0 

reduce :: Tree -> Tree 
reduce t
  | b = reduce t' 
  | otherwise = t' 
  where 
    loop    = split . head . dropWhile ((>4).depth) . iterate explode
    (t', b) = loop t

magnitude :: Tree -> Int 
magnitude (Leaf x) = x 
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r 
   
maxMagnitude :: [Tree] -> Int 
maxMagnitude ts = maximum [ magnitude $ reduce $ add x y | x <- ts, y <- ts, x /= y ]

day18 :: IO ()
day18 = do
    numbers <- map parse' . B.lines <$> B.readFile "inputs/day18.txt"
    let result = foldl1' (\acc n -> reduce $ add acc n) numbers
    print $ magnitude result
    print $ maxMagnitude numbers
