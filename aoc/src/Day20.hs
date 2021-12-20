{-# LANGUAGE OverloadedStrings #-}
module Day20 where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Control.Arrow ((&&&))

parseChars :: Parser Char
parseChars = char '#' <|> char '.'

createMap :: String -> Set Int
createMap = S.fromList . map fst . filter ((=='#').snd) . zip [0..]

createSet :: [String] -> Set (Int, Int)
createSet = S.fromList . concat . zipWith toCoord [0..]
  where
    toCoord x           = mapMaybe (maybeCoord x) . zip [0..]
    maybeCoord x (y, c) = if c=='#' then Just (x,y) else Nothing

myParser :: Parser (Set Int, Set (Int, Int))
myParser = do
    algo <- many' parseChars
    endOfLine >> endOfLine
    charMap <- many' parseChars `sepBy` endOfLine
    pure (createMap algo, createSet charMap)

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) <+> (x, y) = (a+x, b+y)
{-# INLINE (<+>) #-}

miniMax :: Set c -> (c, c)
miniMax = S.findMin &&& S.findMax
{-# INLINE miniMax #-}

rng :: (a -> Int) -> Set a -> (Int, Int)
rng f = (<+> (-1, 1)) . miniMax . S.map f
{-# INLINE rng #-}

step :: Set Int -> (Bool -> Bool) -> Set (Int, Int) -> Set (Int, Int)
step algo f image = S.fromList $ mapMaybe toPixel $ (,) <$> [minX..maxX] <*> [minY..maxY]
    where
        (minX, maxX)   = rng fst image 
        (minY, maxY)   = rng snd image
        toPixel coord  = let dec = foldl' (\acc y -> 2*acc + toBit (coord <+> y)) 0 neighbors
                         in  if f (dec `S.member` algo) then Nothing else Just coord
        toBit coord    = if f (coord `S.member` image) then 1 else 0
        neighbors      = (,) <$> [-1..1] <*> [-1..1]
{-# INLINE step #-}

showImage :: Bool -> Set (Int, Int) -> String
showImage b img = unlines imgLines
    where
        (minX, maxX)   = rng fst img
        (minY, maxY)   = rng snd img
        imgLines       = [[toChar (x, y) | y <- [minY..maxY]] | x <- [minX..maxX]]
        toChar coord   = if coord `S.member` img == b then '#' else '.'

oneCycle :: Set Int -> Set (Int, Int) -> Set (Int, Int)
oneCycle algo = step algo not . step algo id
{-# INLINE oneCycle #-}

part1 :: Set Int -> Set (Int, Int) -> Int
part1 algo = S.size . oneCycle algo

part2 :: Set Int -> Set (Int, Int) -> Int
part2 algo = S.size . (!! 25) . iterate (oneCycle algo)

day20 :: IO ()
day20 = do
    Just (algo, image) <- maybeResult . (`feed` "") . parse myParser <$> B.readFile "inputs/day20.txt"
    print $ part1 algo image
    print $ part2 algo image
