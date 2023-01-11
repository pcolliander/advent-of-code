import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Text.Regex.Posix

toInt :: String -> Int
toInt = read

lineOfSight :: [Int] -> Int -> ([Int], [Int])
lineOfSight treeLine n = (before, after)
  where before = reverse $ take n treeLine
        after  = drop (n + 1) treeLine

parse :: String -> [[Int]]
parse input = map (map toInt . filter (/= "") . splitOn "") $  words input

visible :: Int -> [Int] -> Bool
visible tree trees = all (==True) $ scanl (\acc tree' -> tree > tree') True trees

score :: Int -> [Int] -> Int
score tree [] = 0
score tree (x:xs)
  | tree <= x = 1
  | otherwise = 1 + score tree xs

part1 :: String -> Int
part1 input = length [tree | row <- [0..length rows - 1],
                      column     <- [0..length (head rows) - 1],
                      let tree = (rows !! row) !! column,
                      let treeLineH = rows !! row,
                      let treeLineV = transpose rows !! column,
                      let (before, after) = lineOfSight treeLineH column,
                      let (under, above) = lineOfSight treeLineV row,
                      visible tree after || visible tree before || visible tree under || visible tree above]
  where rows = parse input

part2 :: String -> Int
part2 input =  maximum [scenicScore | row <- [0..length rows - 1],
                      column     <- [0..length (head rows) - 1],
                      let tree = (rows !! row) !! column,
                      let treeLineH = rows !! row,
                      let treeLineV = transpose rows !! column,
                      let (before, after) = lineOfSight treeLineH column,
                      let (under, above) = lineOfSight treeLineV row,
                      let scenicScore = score tree before * score tree after * score tree under * score tree above]
  where rows = parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 input        

