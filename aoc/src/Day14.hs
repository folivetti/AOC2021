module Day14 where 

import Data.Map.Strict ( Map, (!?) ) 
import qualified Data.Map.Strict as M 
import Data.MultiSet (MultiSet) 
import qualified Data.MultiSet as S 
import Control.Arrow ((&&&))

type Rules = Map (Char, Char) Char 

parseInput :: String -> (MultiSet (Char, Char), Rules)
parseInput css = (starterCode, rules) 
    where 
        starterCode = S.fromList $ zip x' (tail x')
        x'        = ' ' : x <> [' ']
        (x:_:rls) = lines css
        rules = M.fromList $ map parseRule rls
        parseRule (a:b:cs) = ((a,b), last cs)
        parseRule _        = error "no parser" 


mostLeastCommon :: MultiSet (Char, Char) -> (Int, Int) 
mostLeastCommon = (maximum &&& minimum) . map (half . snd) . S.toOccurList . S.concatMap toCount 
    where 
        toCount (' ', b) = [b]
        toCount (a, ' ') = [a]
        toCount (a,  b ) = [a,b]
        half             = (`div` 2)


step :: Rules -> MultiSet (Char, Char) -> MultiSet (Char, Char) 
step rules = S.concatMap applyRule 
  where 
    applyRule (a, b) = case rules !? (a, b) of
                         Nothing -> [(a, b)]
                         Just x  -> [(a, x), (x, b)]

polymer :: MultiSet (Char, Char) -> Int 
polymer = uncurry (-) . mostLeastCommon 

day14 :: IO () 
day14 = do 
    (starterCode, rules) <- parseInput <$> readFile "inputs/day14.txt"
    let steps = iterate (step rules) starterCode 
    print $ polymer (steps !! 10)
    print $ polymer (steps !! 40)
