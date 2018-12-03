module D2P1 where
  import Data.List

  forTwo :: [Int] -> (Bool, Bool)
  forTwo is = (elem 2 is, elem 3 is)

  m = (map length) . group . sort
  -- n x = length (filter ((==) x) m)

  processInput :: String -> Int
  processInput input = 
    let
      m = (map length) . group . sort
      n x = length (filter ((==) x) (m input))
    in
      n 2 + n 3

  main :: IO ()
  main = interact (\a -> show (processInput a))