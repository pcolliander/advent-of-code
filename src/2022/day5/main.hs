import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Text.Regex.Posix

toInt :: [String] -> [Int]
toInt = map read

parseStacks stacks = map (filter (/= "")) $ transpose $ map (map (\ (_:_:match:_) -> match) . (\x -> x =~ regex :: [[String]])) $ init $ lines stacks
  where regex =  "(\\[(\\w)\\]| ( ) ) ?"

parseInstruction :: String -> [Int]
parseInstruction instruction = concatMap (toInt . tail) (instruction =~ regex :: [[String]])
  where regex = "move ([0-9]*) from ([0-9]) to ([0-9])"

parse input = (parseStacks stacks, map parseInstruction $ lines instructions)
  where (stacks:instructions:_) = splitOn "\n\n" input

nth :: Int -> [(Int, [String])] -> (Int, [String])
nth 1 xs = head xs
nth n (x:xs) = nth (n-1) xs

applyInstruction :: [Int] -> [(Int, [String])] -> (Int, [String]) -> (Int, [String])
applyInstruction instruction acc numberedStack
  | nr == from = (nr, drop quantity stack)
  | nr == to   = (nr, reverse items ++ stack)
  | otherwise  = numberedStack
  where (quantity: from: to:_) = instruction
        items = take quantity $ snd $ nth from acc
        (nr, stack) = numberedStack

part1 input = intercalate "" $ map (head . snd) finalStacks
  where (stacks,instructions) = parse input
        numberedStacks = zip [1..] stacks
        finalStacks = foldl (\acc instruction -> map (applyInstruction instruction acc) acc) numberedStacks instructions 

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
  -- print $ part2 example
  -- print $ part2 input        
