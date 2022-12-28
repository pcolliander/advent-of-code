import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Char (isSpace)
import Text.Regex.Posix

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseStacks stacks = map (filter (/= "")) $ transpose $ map (map (\ (_:_:match:_) -> match) . (\x -> x =~ regex :: [[String]])) $ init $ lines stacks
  where regex =  "(\\[(\\w)\\]| ( ) ) ?"

parse input = let (stacks:instructions:_) = splitOn "\n\n" input
               in (parseStacks stacks, map trim $ lines instructions)

part1 input = stacks
  where (stacks,instructions) = parse input

main = do  
  example <- readFile "example.txt"
  input <- readFile "input.txt"
  print $ part1 example
  -- print $ part1 input
  -- print $ part2 example
  -- print $ part2 input        
