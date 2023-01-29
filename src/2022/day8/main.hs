import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Text.Regex.Posix
import Control.Monad

toInt :: String -> Int
toInt = read

lineOfSight :: [Int] -> Int -> ([Int], [Int])
lineOfSight treeLine n = (before, after)
  where before = reverse $ take n treeLine
        after  = drop (n + 1) treeLine

parse :: String -> [[Int]]
parse input = map (map toInt . filter (/= "") . splitOn "") $  words input

visible :: Int -> [Int] -> Bool
visible tree trees = and $ scanl (\acc tree' -> tree > tree') True trees

score :: Int -> [Int] -> Int
score tree [] = 0
score tree (x:xs)
  | tree <= x = 1
  | otherwise = 1 + score tree xs

treeWithNeighbours :: [[Int]] -> Int -> Int -> [(Int, [[Int]])]
treeWithNeighbours rows row column = [(tree, [after, before, under, above])]
  where tree = (rows !! row) !! column
        (before, after) = lineOfSight (rows !! row) column
        (under, above) = lineOfSight (transpose rows !! column)  row

part1 :: String -> Int
part1 input = length $ [0..length rows -1]
  >>= \row -> [0..length (head rows) - 1]
  >>= \column -> treeWithNeighbours rows row column
  >>= (\(tree, neighbours) -> guard (any (visible tree) neighbours)  >> return tree)
  where rows = parse input

part2 :: String -> Int
part2 input =  maximum $
  [0..length rows -1]
  >>= \row -> [0..length (head rows) - 1]
  >>= \column -> treeWithNeighbours rows row column
  >>= \(tree, neighbours) -> return $ foldl (\acc n -> acc * score tree n) 1 neighbours
  where rows = parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 input        
