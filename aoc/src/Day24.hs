module Day24 where

import Control.Monad
import Control.Monad.State
import Data.Char (digitToInt, isDigit)
import qualified Data.Set as S 

type MONAD = State (Int, Int, Int, Int, [Char])

getInput :: MONAD Int
getInput = do (w, x, y, z, as) <- get
              put (w,x,y,z, tail as)
              if null as
                 then error "null input stack"
              else pure $ digitToInt $ head as

replace :: String -> Int -> (Int, Int, Int, Int, [Char]) -> (Int, Int, Int, Int, [Char])
replace "w" a (w, x, y, z, as) = (a, x, y, z, as)
replace "x" a (w, x, y, z, as) = (w, a, y, z, as)
replace "y" a (w, x, y, z, as) = (w, x, a, z, as)
replace "z" a (w, x, y, z, as) = (w, x, y, a, as)
replace _ _ _ = error "replace error"

getVal :: String -> (Int, Int, Int, Int, [Char]) -> Int
getVal "w" (w, _, _, _, _) = w
getVal "x" (_, x, _, _, _) = x
getVal "y" (_, _, y, _, _) = y
getVal "z" (_, _, _, z, _) = z
getVal c _ = read c

w = "w"
x = "x"
y = "y"
z = "z"

inp :: String -> MONAD ()
inp c = do
    a <- getInput
    modify (replace c a)

add :: String -> String -> MONAD ()
add a b = do
    v1 <- gets (getVal a)
    v2 <- gets (getVal b)
    modify (replace a (v1+v2))

mul :: String -> String -> MONAD ()
mul a b = do
    v1 <- gets (getVal a)
    v2 <- gets (getVal b)
    modify (replace a (v1*v2))

div' :: String -> String -> MONAD ()
div' a b = do
    v1 <- gets (getVal a)
    v2 <- gets (getVal b)
    modify (replace a (v1 `div` v2))

mod' :: String -> String -> MONAD ()
mod' a b = do
    v1 <- gets (getVal a)
    v2 <- gets (getVal b)
    modify (replace a (v1 `mod` v2))


eql :: String -> String -> MONAD ()
eql a b = do
    v1 <- gets (getVal a)
    v2 <- gets (getVal b)
    modify (replace a (fromEnum (v1==v2)))

-- z = w + 1
-- addX = 11 
-- divZ = 1 
-- addY = 1 
-- x = z `rem` 26 + addX 
-- z = z `div` 1 
-- if x/=w then 26*z + w + addY else z
prog1 :: MONAD ()
prog1 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "11"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "1"
    mul y x
    add z y

-- z = z * 26 + w + 10 
prog2 :: MONAD ()
prog2 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "10"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "10"
    mul y x
    add z y

-- x = 1 
-- y = w + 2  
-- z = 26 * z + w + 2 
prog3 :: MONAD ()
prog3 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "13"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "2"
    mul y x
    add z y

-- z = z `div` 26 * 26  + w + 5 
prog4 :: MONAD ()
prog4 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "-10"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "5"
    mul y x
    add z y


prog5 :: MONAD ()
prog5 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "11"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "6"
    mul y x
    add z y
prog6 :: MONAD ()
prog6 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "11"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "0"
    mul y x
    add z y
prog7 :: MONAD ()
prog7 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "12"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "16"
    mul y x
    add z y
prog8 :: MONAD ()
prog8 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "-11"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "12"
    mul y x
    add z y
prog9 :: MONAD ()
prog9 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "-7"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "15"
    mul y x
    add z y
prog10 :: MONAD ()
prog10 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "1"
    add x "13"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "7"
    mul y x
    add z y
prog11 :: MONAD ()
prog11 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "-13"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "6"
    mul y x
    add z y
prog12 :: MONAD ()
prog12 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "0"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "5"
    mul y x
    add z y
prog13 :: MONAD ()
prog13 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "-11"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "6"
    mul y x
    add z y
prog14 :: MONAD ()
prog14 = do
    inp w
    mul x "0"
    add x z
    mod' x "26"
    div' z "26"
    add x "0"
    eql x w
    eql x "0"
    mul y "0"
    add y "25"
    mul y x
    add y "1"
    mul z y
    mul y "0"
    add y w
    add y "15"
    mul y x
    add z y

progs :: [MONAD ()]
progs = [ prog1, prog2, prog3, prog4, prog5, prog6, prog7
        , prog8, prog9, prog10, prog11, prog12, prog13, prog14
        ]

progs' = [(11,1,1), (10, 1, 10), (13, 1, 2), (-10, 26, 5), (11,1,6), (11,1,0),(12,1,16),(-11,26,12),(-7,26,15),(13,1,7),(-13,26,6),(0,26,5),(-11,26,6),(0,26,15)]

allNums :: String
allNums = "987654321"

execProg :: MONAD () -> Int -> [Char] -> Int
execProg p z inps = getVal "z" $ execState p (0,0,0,z,inps) 

solve :: String -> [(Int, Int, Int)] -> Int -> State (S.Set (Int, Int)) [String]
solve _ [] z        = pure [] 
solve nums (p:ps) z = do 
    seen <- get 
    res <- if S.member k seen
             then pure [] 
             else concat <$> traverse step cands 
    modify (S.insert k) 
    pure res 
  where
      zs    = map (stepFwd p z . digitToInt) nums
      cands = zip zs nums
      k     = (z, length ps)
      step (z', x) = if null ps 
                       then if z' == 0 then pure [[x]] else pure []
                       else map (x:) <$> solve nums ps z'

solve' :: [Int] -> [(Int, Int, Int)] -> Int -> [[Int]] 
solve' _ [] _ = pure []
solve' nums (p:ps) z = do 
    n  <- nums
    z' <- stepBkd p z n
    ns <- solve' nums ps z' 
    pure (n:ns)

stepFwd :: (Int, Int, Int) -> Int -> Int -> Int 
stepFwd (addX, divZ, addY) z w =
  let x  = ((z `rem` 26) + addX)
      z' = z `quot` divZ
  in  if x /= w then (z' * 26) + (w + addY) else z'

stepBkd :: (Int, Int, Int) -> Int -> Int -> [Int] 
stepBkd (addX, divZ, addY) z w =  
    [(x `div` 26) * divZ | let x = z - w - addY, x `mod` 26 == 0 ]
    <> [divZ*z + w - addX | w - addX >= 0, w - addX < 26]

rprogs :: [(Int, Int, Int)]
rprogs = reverse progs' 

solveFwd nums = head $ solve nums progs' 0 `evalState` S.empty 
solveBkw nums = concatMap show . reverse . head $ solve' nums rprogs 0 

day24 :: IO ()
day24 = do
    -- print $ solveFwd "987654321"
    -- print $ solveFwd "123456789"
    print $ solveBkw [9,8..1]
    print $ solveBkw [1..9]
