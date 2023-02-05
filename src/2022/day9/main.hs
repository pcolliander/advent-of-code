import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Control.Monad

type Instruction = (String, Int)

toInstruction :: [String] -> Instruction
toInstruction (l:q:_) = (l, read q)

parse :: String -> [Instruction]
parse input =  map (toInstruction . words) $ filter (/= "") $ splitOn "\n" input

isAdjacent (x1, y1) (x2, y2) = (2 > abs (x2 - x1)) && (2 > abs (y2 - y1))

move step (head, tail) = (head', if isAdjacent head' tail then tail else head)
  where (x, y) = head
        head'
          | step == "R" = (x + 1, y)
          | step == "L" = (x - 1, y)
          | step == "U" = (x, y + 1)
          | step == "D" = (x, y - 1)
          | otherwise = (x, y)

move' step (x, y)  
  | step == "R" = (x + 1, y)
  | step == "L" = (x - 1, y)
  | step == "U" = (x, y + 1)
  | step == "D" = (x, y - 1)
  | otherwise = (x, y)    

motion state (l, quantity) = foldl (\(knot, visited) step -> let knot' = move step knot in (knot', snd knot': visited )) state steps
  where steps = replicate quantity l

determineMove t@(x1, y1) h@(x2, y2)
  | x1 /= x2 && y1 /= y2 = diagonalMove t h
  | otherwise = straightMove t h

diagonalMove (x1, y1) point
  | isAdjacent (x1 + 1, y1 + 1) point = (x1 + 1, y1 + 1)
  | isAdjacent (x1 + 1, y1 - 1) point = (x1 + 1, y1 - 1)
  | isAdjacent (x1 - 1, y1 + 1) point = (x1 - 1, y1 + 1)
  | isAdjacent (x1 - 1, y1 - 1) point = (x1 - 1, y1 - 1)
  | otherwise = (x1, y1)

straightMove (x1, y1) point
  | isAdjacent (x1, y1 + 1) point = (x1, y1 + 1)
  | isAdjacent (x1, y1 - 1) point = (x1, y1 - 1)
  | isAdjacent (x1 - 1, y1)  point = (x1 - 1, y1)
  | isAdjacent (x1 + 1, y1)  point = (x1 + 1, y1)
  | otherwise = (x1, y1)

motion' state (l, quantity) = foldl (\(head:tail, visited) step -> let head' = move' step head
                                                                       knot = foldl (\acc n -> if isAdjacent (last acc) n then acc ++ [n] else acc ++ [determineMove n (last acc)]) [head'] tail
                                                                    in (knot, last knot:visited))
                                                                    state steps
  where steps = replicate quantity l

part1 :: String -> Int
part1 input = length $ Set.fromList $ snd $ foldl motion (knot, visited) instructions
    where instructions = parse input
          knot = ((0,0), (0,0))
          visited = []

part2 :: String -> Int
part2 input = length $ Set.fromList $ snd $ foldl motion' (knot, visited) instructions
    where instructions = parse input
          knot = replicate 10 (0,0)
          visited = []

main = do  
  example <- readFile "example.txt"
  largeExample <- readFile "large-example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 largeExample
  print $ part2 input        
