module Day02 where

data Position = Position { horizontal :: Int
                         , depth      :: Int
                         , aim        :: Int 
                         } deriving Show

data Command = Forward Int | Down Int | Up Int | None
                  deriving Show

applyCommand :: Position -> Command -> Position
applyCommand pos (Forward x) = pos{ horizontal = horizontal pos + x }
applyCommand pos (Down x)    = pos{ depth = depth pos + x }
applyCommand pos (Up x)      = pos{ depth = depth pos - x }
applyCommand pos None        = pos

applyCommandFixed :: Position -> Command -> Position 
applyCommandFixed pos (Forward x) = pos{ horizontal = horizontal pos + x, depth = depth pos + aim pos * x }
applyCommandFixed pos (Down x)    = pos{ aim = aim pos + x }
applyCommandFixed pos (Up x)      = pos{ aim = aim pos - x }
applyCommandFixed pos None        = pos

parseLine :: String -> Command
parseLine l = case words l of
                ["forward", x] -> Forward $ read x
                ["down", x]    -> Down $ read x
                ["up", x]      -> Up $ read x
                _              -> None

day02 :: IO ()
day02 = do
    dat <- map parseLine . lines <$> readFile "inputs/day02.txt"
    let pos   = Position 0 0 0
        part1 = foldl applyCommand pos dat
        part2 = foldl applyCommandFixed pos dat
    print $ horizontal part1 * depth part1
    print $ horizontal part2 * depth part2
