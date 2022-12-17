import Data.List

data Shape = Rock | Paper | Scissor 
  deriving (Eq, Show)

score :: Shape -> Int
score a = case a of
  Rock -> 1
  Paper -> 2
  Scissor -> 3

mapping :: [Char] -> Shape
mapping "A" = Rock
mapping "B" = Paper
mapping "C" = Scissor
mapping "X" = Rock
mapping "Y" = Paper
mapping "Z" = Scissor

play :: [Shape] -> Int
play (opponent:you:_)
  | opponent == losesTo you = scoreByMove
  | opponent == you = scoreByMove + 3
  | otherwise = scoreByMove + 6
  where scoreByMove = score you

losesTo :: Shape -> Shape
losesTo a = case a of
  Rock -> Paper 
  Paper -> Scissor
  Scissor -> Rock 

winsOver :: Shape -> Shape
winsOver a = case a of
  Rock -> Scissor
  Paper -> Rock
  Scissor -> Paper

newGame :: [String] -> Int
newGame (opponent:you:_)
  | you == "X" =  score loseWith
  | you == "Y" =  score opponentMove + 3
  | otherwise = score winWith + 6
  where opponentMove = mapping opponent
        loseWith = winsOver opponentMove
        winWith = losesTo opponentMove

parse :: String -> [[String]]
parse input = map words $ lines input

part1 :: String -> Int
part1 input = sum $ map (play . map mapping) $ parse input

part2 :: String -> Int
part2 input = sum $ map newGame $ parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 input        

