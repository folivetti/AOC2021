module Day02 where

import Data.Foldable (fold)

data Position = Position { horizontal :: Int
                         , depth      :: Int
                         , aim        :: Int 
                         } deriving (Show)

instance Semigroup  Position where 
    Position x y z <> Position a b c = Position (x+a) (y+b + z*a) (z+c)
instance Monoid Position where 
    mempty = Position 0 0 0 

parseLine :: String -> Position
parseLine l = case words l of
                ["forward", x] -> Position (read x) 0 0 
                ["down", x]    -> Position 0 (read x) 0
                ["up", x]      -> Position 0 (negate $ read x) 0
                _              -> Position 0 0 0
parseLineFix :: String -> Position
parseLineFix l = case words l of
                ["forward", x] -> Position (read x) 0 0 
                ["down", x]    -> Position 0 0 (read x)
                ["up", x]      -> Position 0 0 (negate $ read x)
                _              -> Position 0 0 0

day02 :: IO ()
day02 = do
    dat <- lines <$> readFile "inputs/day02.txt"
    let pos   = Position 0 0 0
        part1 = fold (pos: map parseLine dat)
        part2 = fold (pos: map parseLineFix dat)
    print $ horizontal part1 * depth part1
    print $ horizontal part2 * depth part2
