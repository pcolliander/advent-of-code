import Data.List
import Data.List.Split
import qualified Data.Set as Set

toInt :: [String] -> [Int]
toInt = map read

overlapping :: Ord a => [[a]] -> Bool
overlapping [l1, l2] =
  Set.isSubsetOf(Set.fromList l1) (Set.fromList l2)
  || Set.isSubsetOf(Set.fromList l2) (Set.fromList l1)

parse :: String -> [[[Int]]]
parse input = map (map (toInt . splitOn "-") . splitOn ",") $ lines input

ranges :: [Int] -> [Int]
ranges [a,b] = [a..b]

part1 :: String -> Int
part1 input =  length $ filter id $ map (overlapping . map ranges) $ parse input

-- part2 :: String -> Int
-- part2 input = sumPriorities $ map overlapping' $ chunksOf 3 $ lines input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  -- print $ part2 example
  -- print $ part2 input        
