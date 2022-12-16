import Data.List.Split
import Data.List

toInt :: [String] -> [Int]
toInt = map read

parse :: String -> [[Int]]
parse input = map (toInt . words) $ splitOn "\n\n" input

prop1 :: String -> Int
prop1 input = maximum $ map sum $ parse input

prop2 :: String -> Int
prop2 input = sum $ take 3 $ reverse $ sort $ map sum $ parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ prop1 example
  print $ prop1 input
  print $ prop2 example
  print $ prop2 input
