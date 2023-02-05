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

-- try State monad.

-- (flip move)
motion state (l, quantity) = foldl (\(knot, visited) step -> let knot' = move step knot in (knot', snd knot': visited )) state steps
  where steps = replicate quantity l

part1 :: String -> Int
part1 input = length $ Set.fromList $ snd $ foldl (\acc instruction -> motion acc instruction) (knot, visited) instructions
    where instructions = parse input
          knot = ((0,0), (0,0))
          visited = []

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  -- print $ part2 example
  -- print $ part2 input        
