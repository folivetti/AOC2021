{-# LANGUAGE OverloadedStrings #-}
module Day22 where

import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.MultiSet (MultiSet, union, (\\))
import qualified Data.MultiSet as S
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.List (foldl')

data Action      = On | Off deriving Show
type Interval    = (Int, Int)
type Cuboid      = (Interval, Interval, Interval)
type Instruction = (Action, Cuboid)
type Engine      = (MultiSet Cuboid, MultiSet Cuboid)

intersect :: Interval -> Interval -> Maybe Interval
intersect (x, y) (a, b)
  | maxLo <= minHi = Just (maxLo, minHi)
  | otherwise      = Nothing
  where
    maxLo = max x a
    minHi = min y b
{-# INLINE intersect #-}

card :: Interval -> Int
card (x, y) = y - x + 1
{-# INLINE card #-}

intersectCube :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCube (x, y, z) (a, b, c) = do
    xa <- x `intersect` a
    yb <- y `intersect` b
    zc <- z `intersect` c
    pure (xa, yb, zc)
{-# INLINE intersectCube #-}

cardCube :: Cuboid -> Int
cardCube (x, y, z) = card x * card y * card z
{-# INLINE cardCube #-}

cardEngine :: Engine -> Int 
cardEngine (ons, offs) = sum (S.map cardCube ons) - sum (S.map cardCube offs) 
{-# INLINE cardEngine #-}

applyAction :: Engine -> (Action, Cuboid) -> Engine
applyAction (ons, offs) (act, c) =
    case act of
      On  -> (S.insert c ons', offs')
      Off -> (ons', offs')
  where
    subInter = S.mapMaybe (intersectCube c) offs -- every valid intersection with c 
    addInter = S.mapMaybe (intersectCube c) ons
    ons'     = subInter `union` ons  -- (c /\ offs) \/ ons, if act==Off, add intersections with offs so we don't remove them twice, if it's on, do the same but add back `c`
    offs'    = addInter `union` offs -- (c /\ ons) \/ offs, if act==Off, add intersections with on, we don't need to store anything else since we'll be turning off. If act==On, we do the same because we will be adding it into ons 
{-# INLINE applyAction #-}

parseAction :: Parser Action
parseAction = do
    act <- string "on" <|> string "off"
    case act of
      "on"  -> pure On
      "off" -> pure Off
      _     -> error $ "no parser for action " <> B.unpack act

parseRange :: Parser (Int, Int)
parseRange = do
    string "x=" <|> string "y=" <|> string "z="
    lo <- signed decimal
    string ".."
    hi <- signed decimal
    pure (lo, hi)

myParser :: Parser Instruction
myParser = do
    act <- parseAction
    skipSpace
    x <- parseRange
    char ','
    y <- parseRange
    char ','
    z <- parseRange
    pure (act, (x,y,z))

parseLine :: ByteString -> Maybe Instruction
parseLine = maybeResult . (`feed` "") . parse myParser

parseDat :: [ByteString] -> [Instruction]
parseDat = mapMaybe parseLine

applyInstructions :: [Instruction] -> Engine 
applyInstructions = foldl' applyAction (S.empty, S.empty) 

solve :: [Instruction] -> Int
solve = cardEngine . applyInstructions

day22 :: IO ()
day22 = do
    dat <- B.lines <$> B.readFile "inputs/day22.txt"
    let instructions = parseDat dat
    print $ solve $ take 20 instructions
    print $ solve instructions
