import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Text.Regex.Posix

parse :: String -> [String]
parse input = filter (/= "") $ splitOn "" input

startOfPackage :: Int -> [String] -> Int
startOfPackage count full@(_:xs)
  | 4 == packageLength = count + 4
  | otherwise = startOfPackage (count + 1) xs
  where packageLength = length $ Set.fromList(take 4 full)

startOfMessage :: Int -> [String] -> Int
startOfMessage count full@(_:xs)
  | 14 == packageLength = count + 14
  | otherwise = startOfMessage (count + 1) xs
  where packageLength = length $ Set.fromList(take 14 full)

part1 :: String -> Int
part1 input = startOfPackage 0 $ parse input

part2 :: String -> Int
part2 input = startOfMessage 0 $ parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  print $ part2 example
  print $ part2 input        
