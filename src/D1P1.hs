module D1P1 where

  sumInput :: String -> Integer
  sumInput = sum . (map read) . (map . filter) ((/=) '+') . words

  main :: IO ()
  main = interact (show . sumInput)
