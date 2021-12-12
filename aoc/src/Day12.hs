module Day12 where 

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as M 
import Data.List.Split (splitOn) 
import Data.Char (isLower) 
import Data.MultiSet (MultiSet, (\\), occur, fromList, toList, singleton)
import qualified Data.MultiSet as S 
import Data.List (foldl')

data Cave = Start | Large String | Small String | End 
    deriving (Show, Read, Eq, Ord) 

type Graph = Map Cave (MultiSet Cave)
type Path  = [Cave] 

type Candidates = Path -> [Cave]

getEdgeName :: String -> [(Cave, MultiSet Cave)]
getEdgeName = toTuples . map toCave . splitOn "-" 
  where toTuples [x,y] = [(x, singleton y), (y, singleton x)]
        toTuples xs    = error $ "Parser error: " <> show xs

toCave :: String -> Cave 
toCave "start" = Start 
toCave "end"   = End 
toCave (c:cs)  | isLower c = Small (c:cs)
               | otherwise = Large (c:cs)
toCave x = error $ "Parser error: " <> show x

parseLine :: String -> [Graph] 
parseLine = map (uncurry M.singleton) . getEdgeName 

parseFile :: String -> Graph 
parseFile = M.unionsWith (<>) . concatMap parseLine . lines 

isSmall :: Cave -> Bool
isSmall (Small _) = True 
isSmall Start     = True 
isSmall End       = True 
isSmall _         = False 
{-# INLINE isSmall #-}

onlyOnce :: Graph -> Candidates 
onlyOnce g p = toList . (g ! head p \\) . S.filter isSmall . fromList $ p 
{-# INLINE onlyOnce #-}

canTwice :: Graph -> Candidates 
canTwice g p = if hasTwice p'
                 then toList $ n \\ S.filter isSmall p'
                 else toList $ n \\ fromList [Start]
    where
        n         = g ! head p 
        p'        = fromList p
        isTwice x = isSmall x && (occur x p' == 2)
        hasTwice  = not . S.null . S.filter isTwice
{-# INLINE canTwice #-}

dfs :: Candidates -> Int
dfs candidates = go [Start] 
  where
    go :: [Cave] -> Int
    go []          = 0
    go (End:path)  = 1
    go path        = foldl' (\acc -> (acc +) . go . (:path)) 0
                   $ candidates path 

day12 :: IO ()
day12 = do 
    graph <- parseFile <$> readFile "inputs/day12.txt"
    print $ dfs (onlyOnce graph)
    print $ dfs (canTwice graph)
