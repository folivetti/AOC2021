module Day12 where 

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as M 
import Data.List.Split (splitOn) 
import Data.Char (isLower) 
import Data.List (foldl', (\\))

data Cave = Start | Large String | Small String | End 
    deriving (Show, Read, Eq, Ord) 

type Graph = Map Cave [Cave]
type Path  = [Cave] 

type Candidates = Path -> [Cave]

getEdgeName :: String -> [(Cave, [Cave])]
getEdgeName = toTuples . map toCave . splitOn "-" 
    where 
        toTuples [Start, y] = [(Start, [y])]
        toTuples [y, Start] = [(Start, [y])]
        toTuples [x,y]      = [(x, [y]), (y, [x])]
        toTuples xs         = error $ "Parser error: " <> show xs

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

dfs :: Graph -> Bool -> Int
dfs graph = go [Start]
  where
    isNotTabu p n = not (isSmall n && n `elem` p)

    go :: [Cave] -> Bool -> Int
    go [] _         = 0
    go (End:path) _ = 1
    go path False   = foldl' (\acc n -> acc + go (n:path) False) 0 $ filter (isNotTabu path) $ graph ! head path
    go path True    = foldl' (\acc n -> acc + go (n:path) (isNotTabu path n)) 0 $ graph ! head path

day12 :: IO ()
day12 = do 
    graph <- parseFile <$> readFile "inputs/day12.txt"
    print $ dfs graph False
    print $ dfs graph True
