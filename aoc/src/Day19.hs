{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day19 where 

import Data.Attoparsec.ByteString.Char8 
import qualified Data.ByteString.Char8 as B 
import Control.Applicative ((<|>)) 
import Data.Either (partitionEithers)
import Data.List (transpose, nub) 
import qualified Data.Map.Strict as M 

data Point = P Int Int Int deriving (Show, Eq, Ord) 
type Scanner = [Point]

instance Num Point where 
    (P x y z) + (P a b c) = P (x+a) (y+b) (z+c)
    (P x y z) * (P a b c) = P (x*a) (y*b) (z*c)
    abs (P x y z)         = P (abs x) (abs y) (abs z)
    fromInteger a         = let x = fromInteger a in P x x x
    negate (P x y z)      = P (-x) (-y) (-z)
    signum (P x y z)      = 0

myParser :: Parser [Scanner] 
myParser = parseScanner `sepBy` "\n\n"

parseScanner :: Parser Scanner 
parseScanner = do 
    "--- scanner " >> decimal >> " ---" >> endOfLine 
    parsePoint `sepBy` endOfLine

parsePoint :: Parser Point 
parsePoint = do 
    [x,y,z] <- signed decimal `sepBy` char ','
    pure $ P x y z

runParser dat = case parse myParser dat of 
                  Done _ x -> x
                  Partial p -> case p "" of 
                                 Done _ x -> x 
                                 _ -> error "still no parser" 
                  _ -> error "no parser"

align :: [(Scanner, Point)] -> [Scanner] -> [Scanner] -> [(Scanner, Point)]
align res _ [] = res 
align res (cur:refs) scanners = align (found <> res) (map fst found <> refs) notFound 
  where 
    (found, notFound)  = partitionEithers [ eitherHead sc $ checkAngles cur sc | sc <- scanners]
    eitherHead x []    = Right x 
    eitherHead _ (x:_) = Left x 
    checkAngles x y    = [(map (pos+) a, pos) | a <- angles y, pos <- overlap x a]
    angles = transpose . map rots
    rots p = scanl (flip ($)) p steps 
      where 
        steps = [r,t,t,t,r,t,t,t,r,t,t,t, r.t.r.r, t,t,t,r,t,t,t,r,t,t,t]
        r (P x y z) = P x z (-y) 
        t (P x y z) = P (-y) x z 

overlap :: Scanner -> Scanner -> [Point]
overlap as bs = M.keys . M.filter (>= 12) . M.fromListWith (+) . map (, 1) $ 
               (-) <$> as <*> bs

manhattan :: Point -> Point -> Int 
manhattan x y = let tot (P x y z) = x+y+z in tot $ abs (x-y)

pickSolution :: [a] -> [(a,a)]
pickSolution [] = [] 
pickSolution (x:xs) = map (x,) xs <> pickSolution xs 

day19 :: IO () 
day19 = do
    (x:xs) <- runParser <$> B.readFile  "inputs/day19.txt"
    let aligned = align [(x, 0)] [x] xs
        part1 = length . nub . concatMap fst $ aligned
        part2 = maximum . map (uncurry manhattan) . pickSolution . map snd $ aligned
    print part1 
    print part2
