module D1P1 where

  processInput :: String -> Integer
  -- processInput = sum . (map read) . (map (filter ((/=) '+'))) . words
  processInput = sum . (map read) . (map . filter) ((/=) '+') . words

  main :: IO ()
  main = interact (show . processInput)
