module Day06 where 

import Data.Vector hiding (zip, map, mapM_)
import qualified Data.IntMap.Strict as M 
import Prelude hiding (sum)
import Data.List.Split (splitOn)

example :: [Int] 
example = [3,4,3,1,2]

createInput :: [Int] -> Vector Int 
createInput xs = generate 9 f
    where 
        mxs = M.fromListWith (+) $ zip xs (repeat 1)
        f ix = if M.member ix mxs 
                  then mxs M.! ix 
                  else 0 

step :: Vector Int -> Vector Int 
step xs = generate 9 f
    where 
        f ix 
          | ix < 8 && ix /= 6 = xs ! (ix+1) 
          | ix == 6              = xs ! 7 + xs ! 0 
          | otherwise            = xs ! 0 


day06 :: IO () 
day06 = do
    dat <- map read . splitOn "," <$> readFile "inputs/day06.txt"
    let sim = iterate step $ createInput dat
    print $ sum $ sim !! 80
    print $ sum $ sim !! 256
