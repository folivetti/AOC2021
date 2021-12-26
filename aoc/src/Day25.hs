module Day25 where

import Data.Set (Set)
import qualified Data.Set as S

parse :: [String] -> (Set (Int, Int), Set (Int, Int))
parse rows = (S.fromList rights, S.fromList downs)
  where
      rights    = map toTuple $ filter (\(_,_,c) -> c == '>') cucumbers
      downs     = map toTuple $ filter (\(_,_,c) -> c == 'v') cucumbers
      toTuple (x,y,_) = (x,y)
      cucumbers = concat $ zipWith parseRow [0..] rows
      parseRow :: Int -> String -> [(Int, Int, Char)]
      parseRow ix cols = concat $ zipWith parseCol [0..] cols
        where
            parseCol :: Int -> Char -> [(Int, Int, Char)]
            parseCol iy c = [(ix,iy,c) | c == '>' || c == 'v']

step :: Int -> Int -> (Set (Int, Int), Set (Int, Int)) -> (Set (Int, Int), Set (Int, Int))
step nrows ncols (rights, downs) = (rights', downs')
    where
        rights'         = S.map moveRight rights 
        downs'          = S.map moveDown downs 
        moveRight (x,y) = let y' = if y+1 == ncols then 0 else y+1
                          in  if S.member (x,y') rights || S.member (x,y') downs
                                 then (x,y)
                                 else (x,y')
        moveDown (x,y)  = let x' = if x+1 == nrows then 0 else x+1
                          in  if S.member (x',y) rights' || S.member (x',y) downs
                                 then (x,y)
                                 else (x',y)

printGrid nrows ncols (rights, downs) = unlines [[printChar (x,y) | y <- [0..ncols-1]] | x <- [0..nrows-1]]
    where
        coords = (,) <$> [0..nrows-1] <*> [0..ncols-1]
        printChar xy
          | S.member xy rights = '>'
          | S.member xy downs  = 'v'
          | otherwise          = '.'

part1 = (1+) . fst . head . filter snd . zipWith (\ix (a,b) -> (ix, a==b)) [0..] 

day25 :: IO ()
day25 = do
    dat <- lines <$> readFile "inputs/day25.txt"
    let cucumbers = parse dat
        nrows     = length dat
        ncols     = length $ head dat
        stp       = step nrows ncols 
        prnt      = printGrid nrows ncols 
        steps     = iterate stp cucumbers
        findStop  = part1 $ zip steps (tail steps)

    print findStop
