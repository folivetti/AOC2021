module Day09 where

import qualified Data.ByteString.Char8 as B
import Data.Char ( digitToInt )
import Data.Array
import Data.List ( sortOn ) 
import Data.Ord (Down(..))
import qualified Data.Set as S 
import Control.Monad.State 

type Heatmap = Array (Int, Int) Int

mkArray :: [[Int]] -> Heatmap
mkArray xs = listArray ((0,0), hi) $ concat xs
    where
        hi = (length xs - 1, length (head xs) - 1)

inBound :: Heatmap -> (Int, Int) -> Bool 
inBound xs (x, y) = x >= lo_x && x <= hi_x && y >= lo_y && y <= hi_y
    where 
        ((lo_x, lo_y), (hi_x, hi_y)) = bounds xs

neighs :: Heatmap -> (Int, Int) -> [(Int, Int)]
neighs xs (x, y) = filter (inBound xs) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

lowPoints :: Heatmap -> [((Int, Int), Int)]
lowPoints xs = [ (ix, e) | (ix, e) <- assocs xs, isLocalMin ix e ]
  where
     isLocalMin ix e = e < minimum (map (xs!) $ neighs xs ix)

riskLevel :: [Int] -> [Int]
riskLevel = map (+1)

floodFill :: Heatmap -> (Int, Int) -> [Int]
floodFill xs ix = go ix `evalState` S.empty
  where 
    go :: (Int, Int) -> State (S.Set (Int, Int)) [Int] 
    go iy@(x, y) = do 
        tabu <- get 
        let queue = [ iz | iz <- neighs xs iy
                    , xs ! iz > xs ! iy
                    , xs ! iz /= 9 
                    , iz `S.notMember` tabu
                    ]
        put $ S.union tabu $ S.fromList $ iy : queue
        region <- traverse go queue 
        pure $ xs ! iy : concat region 


part1 :: Heatmap -> Int
part1 = sum . riskLevel . map snd . lowPoints

part2 :: Heatmap -> Int
part2 xs = product . take 3 . sortOn Down 
         $ map (length . floodFill xs . fst) (lowPoints xs)

day09 :: IO ()
day09 = do
    dat <- B.lines <$> B.readFile  "inputs/day09.txt"
    let myInput = mkArray $ map (B.foldr' (\c xs -> digitToInt c : xs) []) dat
    print $ part1 myInput
    print $ part2 myInput
