import System.IO  
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

doubleMe x = x * 2


x = [1, -2, 3, 1]

  
f :: [String] -> [Int]
f = map read


-- main = do  
--   handle <- openFile "input.txt" ReadMode  
--   contents <- hGetContents handle  
--   print $ sum contents  
--   hClose handle

main = do
  ls <- fmap Text.lines (Text.readFile "input.txt")
  print $ f ls
  -- print $ sum x
  -- print $ doubleMe 16
  -- print $ doubleMe 20
  -- let x = read "123" :: Integer
  -- print x
