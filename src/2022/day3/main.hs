import Data.List
import qualified Data.Set as Set

getPriority :: Char -> Int
getPriority a = head [p | (l, p) <- zip (['a'..'z'] ++ ['A'..'Z']) [1..], a == l] 

compartments :: String -> (String, String)
compartments l = splitAt ((length l + 1) `div` 2) l

overlapping :: Ord a => ([a], [a]) -> Set.Set a
overlapping (l1, l2) = Set.intersection(Set.fromList l1) (Set.fromList l2)

part1 :: String -> Int
part1 input = sum $ map getPriority $ intercalate "" $ map (Set.toList . overlapping . compartments) $ lines input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  -- print $ part2 example
  -- print $ part2 input        

