{-# LANGUAGE  TupleSections #-}
module Day23 where 

import Data.Map.Strict (Map, fromList, toList, member, notMember, (!?), (!), insert, delete, empty, mapMaybeWithKey) 
import Control.Monad.State  
import Data.Maybe (catMaybes, mapMaybe) 
import qualified Data.Set as S 
import Data.OrdPSQ (OrdPSQ) 
import qualified Data.OrdPSQ as P

data Amphipod = A | B | C | D deriving (Show, Eq, Ord)
data Place    = Stack Amphipod | Space Int deriving (Show, Eq, Ord)
type Burrow   = (Map Amphipod [Amphipod], Map Int (Maybe Amphipod)) -- stack, space 

roomToHall :: Int -> Burrow -> [(Burrow, Int)]
roomToHall maxDepth (stack, space) = do 
    (amph, a:as) <- toList stack
    guard $ not . all (==amph) $ (a:as)
    (x, Nothing) <- toList space 
    guard $ null (blockers (amphToInt amph) x space)
    let distance = maxDepth - length as + dist amph x 
        energy   = cost a * distance  
        stack'   = insert amph as stack 
        space'   = insert x (Just a) space  
    pure ((stack', space'), energy)
{-# INLINE roomToHall #-}

hallToRoom :: Int -> Burrow -> [(Burrow, Int)]
hallToRoom maxDepth (stack, space) = do 
    (x, Just amph) <- toList space 
    let as = stack ! amph 
    guard $ all (==amph) as 
    guard $ null (blockers x (amphToInt amph) space)
    let distance = maxDepth - length as + dist amph x 
        energy   = cost amph * distance 
        stack'   = insert amph (amph:as) stack 
        space'   = insert x Nothing space 
    pure ((stack', space'), energy)
{-# INLINE hallToRoom #-}

neighbors :: Int -> Burrow -> [(Burrow, Int)]
neighbors maxDepth = hallToRoom maxDepth <> roomToHall maxDepth 
{-# INLINE neighbors #-}

cost :: Amphipod -> Int 
cost A = 1 
cost B = 10 
cost C = 100 
cost D = 1000 
{-# INLINE cost #-}

dist :: Amphipod -> Int -> Int 
dist a x = abs (x - amphToInt a) 
{-# INLINE dist #-}

amphToInt :: Amphipod -> Int 
amphToInt A = 2 
amphToInt B = 4 
amphToInt C = 6 
amphToInt D = 8 
{-# INLINE amphToInt #-}

blockers :: Int -> Int -> Map Int (Maybe Amphipod) -> [Int] 
blockers x y = map fst . toList . mapMaybeWithKey toBlck  
  where 
      toBlck z c = if x < z && y > z || x > z && y < z
                     then c 
                     else Nothing 
{-# INLINE blockers #-}

st0,st1, st0', st1' :: Burrow
st0  = (fromList [(A, [A,D]), (B, [C,A]), (C, [B, D]), (D, [C, B])], fromList $ zip [0,1,3,5,7,9,10] $ repeat Nothing)
st0' = (fromList [(A, [A,D,D,D]), (B, [C,C,B,A]), (C, [B,B,A,D]), (D, [C,A,C,B])], fromList $ zip [0,1,3,5,7,9,10] $ repeat Nothing)
st1  = (fromList [(A, [A,A]), (B, [B,B]), (C, [C, C]), (D, [D, D])], fromList $ zip [0,1,3,5,7,9,10] $ repeat Nothing)
st1' = (fromList [(A, [A,A,A,A]), (B, [B,B,B,B]), (C, [C,C,C,C]), (D, [D,D,D,D])], fromList $ zip [0,1,3,5,7,9,10] $ repeat Nothing)

search :: Int -> OrdPSQ Burrow Int Int -> Burrow -> S.Set Burrow -> Int 
search maxDepth queue goal seen
  | next == goal = totEnergy 
  | otherwise    = search maxDepth queue' goal seen' 
  where 
    Just (next, totEnergy, _, q) = P.minView queue
    seen'                        = S.insert next seen
    queue'                       = foldr (\(b, e) acc -> insertIfMin b (totEnergy + e) (totEnergy + e) acc) q neighs
    neighs                       = filter (not . (`S.member` seen) . fst) $ neighbors maxDepth next

insertIfMin k c v q = case P.lookup k q of  
                        Nothing -> P.insert k c v q 
                        Just (c',_) -> if c' < c then q else P.insert k c v q
{-# INLINE insertIfMin #-}

day23 :: IO ()
day23 = do 
    let e1 = search 2 (P.singleton st0 0 0) st1 S.empty 
    let e2 = search 4 (P.singleton st0' 0 0) st1' S.empty 
    print e1
    print e2
