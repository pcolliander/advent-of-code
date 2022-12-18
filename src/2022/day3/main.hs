import Data.List
import Data.List.Split
import qualified Data.Set as Set

getPriority :: Char -> Int
getPriority a = head [p | (l, p) <- zip (['a'..'z'] ++ ['A'..'Z']) [1..], a == l] 

compartments :: String -> (String, String)
compartments l = splitAt ((length l + 1) `div` 2) l

overlapping :: Ord a => ([a], [a]) -> [a]
overlapping (l1, l2) = Set.toList $ Set.intersection(Set.fromList l1) (Set.fromList l2)

overlapping' :: Ord a => [[a]] -> [a]
overlapping' (l1: l2: l3:_) = Set.toList $ Set.intersection(Set.fromList l1) $ Set.intersection (Set.fromList l2) (Set.fromList l3)

sumPriorities :: [String] -> Int
sumPriorities l = sum $ map getPriority $ intercalate "" l

part1 :: String -> Int
part1 input = sumPriorities $ map (overlapping . compartments) $ lines input

part2 :: String -> Int
part2 input = sumPriorities $ map overlapping' $ chunksOf 3 $ lines input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 input        
