module Day21 where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!?))

data GameState = GS { _p1       :: Int
                    , _p2       :: Int
                    , _score1   :: Int
                    , _score2   :: Int
                    , _lastRoll :: Int
                    , _turn     :: Int
                    } deriving (Show, Eq, Ord)

type Game = StateT GameState IO
type Dirac = StateT (Map GameState (Int, Int)) IO

data Player = P1 | P2 deriving Show

rollDice :: Game Int
rollDice = do
    n <- gets _lastRoll
    let n' = if n==100 then 1 else n+1
    modify (\gt -> gt{ _lastRoll = n' })
    modify incTurn
    pure n'

incTurn :: GameState -> GameState
incTurn gt = gt{ _turn = _turn gt + 1 }

diracDice :: Dirac [Int]
diracDice = pure [1,2,3]

nextMove :: Game Int -> Game Int
nextMove f = do
    x <- f
    y <- f
    z <- f
    pure (x+y+z)

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

nextMoves :: Dirac [Int] -> Dirac [Int]
nextMoves f = do
    x <- f
    y <- f
    z <- f
    pure $ add <$> x <*> y <*> z

nextPlace :: Int -> Int -> Int
nextPlace move p = (move + p - 1) `mod` 10 + 1

play :: Player -> Game Int
play P1 = do
    move <- nextMove rollDice
    p1   <- gets _p1
    let next = nextPlace move p1
    modify (\gt -> gt{ _p1 = next, _score1 = _score1 gt + next })
    gets _score1
play P2 = do
    move <- nextMove rollDice
    p2   <- gets _p2
    let next = nextPlace move p2
    modify (\gt -> gt{ _p2 = next, _score2 = _score2 gt + next })
    gets _score2

playGame :: Game Int
playGame = do
    sc1 <- play P1
    if sc1 >= 1000
       then do t <- gets _turn
               gets ((*t) . _score2)
       else do sc2 <- play P2
               if sc2 >= 1000
                  then do t <- gets _turn
                          gets ((*t) . _score1)
                  else playGame

playDirac :: GameState -> Dirac (Int, Int)
playDirac gs@(GS p1 p2 sc1 sc2 r t)
  | sc1 >= 21 = pure (1, 0)
  | sc2 >= 21 = pure (0, 1)
  | otherwise = do
      cache <- get
      case cache !? gs of
        Just scores -> pure scores
        _           -> do scores <- mapM (playDirac . swapG . change gs) moves
                          let totScore = swap $ foldr addTuple (0,0) scores
                          modify (M.insert gs totScore)
                          pure totScore

other :: Player -> Player
other P1 = P2
other P2 = P1

moves :: [Int]
moves = sum <$> replicateM 3 [1,2,3]

addTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuple (x,y) (a,b) = (x+a, y+b)

swapG (GS p1 p2 sc1 sc2 r t) = GS p2 p1 sc2 sc1 r t 
swap (a,b) = (b,a) 

change :: GameState -> Int -> GameState
change (GS p1 p2 sc1 sc2 r t) move = let p1'  = nextPlace move p1
                                         sc1' = sc1 + p1'
                                     in  GS p1' p2 sc1' sc2 r t

day21 :: IO ()
day21 = do
    let stGame = GS 9 10 0 0 0 0
    game1 <- playGame `runStateT` stGame
    game2 <- playDirac stGame `evalStateT` M.empty
    print game1
    print game2
