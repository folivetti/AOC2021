{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import Data.Char ( digitToInt )
import Data.List ( foldl' )
import Data.Attoparsec.ByteString.Char8 hiding ( take )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as C
import Control.Applicative ((<|>))

data Packet = Lit Int Int | Op Int Int [Packet] 
                deriving Show 

hexToBin :: Char -> ByteString  
hexToBin '0' = "0000" 
hexToBin '1' = "0001" 
hexToBin '2' = "0010" 
hexToBin '3' = "0011" 
hexToBin '4' = "0100" 
hexToBin '5' = "0101" 
hexToBin '6' = "0110" 
hexToBin '7' = "0111" 
hexToBin '8' = "1000" 
hexToBin '9' = "1001" 
hexToBin 'A' = "1010" 
hexToBin 'B' = "1011" 
hexToBin 'C' = "1100" 
hexToBin 'D' = "1101" 
hexToBin 'E' = "1110" 
hexToBin 'F' = "1111" 
hexToBin _   = "" 

binToDec :: String -> Int 
binToDec = foldl' (\acc d -> digitToInt d + 2*acc) 0 

parsePacket :: Parser Packet 
parsePacket = try parseLiteral <|> parseOperator

parseOperator :: Parser Packet 
parseOperator = do 
    version <- binToDec <$> count 3 digit 
    typeID  <- binToDec <$> count 3 digit 
    lenType <- count 1 digit
    if lenType == "0" 
       then Op version typeID <$> parseTotalLen 
       else Op version typeID <$> parseNumPackets 

parseTotalLen :: Parser [Packet]
parseTotalLen = do 
    len <- binToDec <$> count 15 digit 
    dat <- C.pack <$> count len digit 
    case parse (many1 parsePacket) dat of
      Done _ packets -> pure packets 
      Partial i      -> case i "" of 
                          Done _ pckts -> pure pckts 
                          _            -> error "giving up"
      Fail i c b     -> error $ concat c <> b

parseNumPackets :: Parser [Packet]
parseNumPackets = do 
    n <- binToDec <$> count 11 digit 
    count n parsePacket 

parseLiteral :: Parser Packet 
parseLiteral = do 
    version <- binToDec <$> count 3 digit 
    typeID  <- string "100"
    x       <- binToDec <$> parseNumber
    pure $ Lit version x 

parseNumber :: Parser String 
parseNumber = do 
    bit <- digit 
    x   <- count 4 digit    
    if bit == '0' 
       then pure x
       else (x <>) <$> parseNumber 

sumOfVersions :: Packet -> Int 
sumOfVersions (Lit v _) = v 
sumOfVersions (Op v _ ps) = v + sum (map sumOfVersions ps)

eval :: Packet -> Int 
eval (Lit _ x)    = x 
eval (Op _ op ps) = 
    case op of 
      0 -> foldl' (\acc x -> acc + eval x) 0 ps 
      1 -> foldl' (\acc x -> acc * eval x) 1 ps 
      2 -> minimum $ map eval ps
      3 -> maximum $ map eval ps
      5 -> gt $ take 2 $ map eval ps 
      6 -> lt $ take 2 $ map eval ps 
      7 -> eq $ take 2 $ map eval ps 

gt [x,y] = if x > y then 1 else 0
lt [x,y] = if x < y then 1 else 0
eq [x,y] = if x == y then 1 else 0


day16 :: IO ()
day16 = do
    dat <- C.concatMap hexToBin <$> C.readFile "inputs/day16.txt"
    let pckts = parse parsePacket dat 
    print $ sumOfVersions <$> pckts
    print $ eval <$> pckts
