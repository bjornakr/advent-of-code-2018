module D2P1 where
  import Data.List

  -- forTwo :: [Int] -> (Bool, Bool)
  -- forTwo is = (elem 2 is, elem 3 is)

  count :: String -> [Int]
  count = (map length) . group . sort

  occurs :: Int -> [Int] -> Int
  occurs x = length . (filter ((==) x))

  occurs2 :: [Int] -> [Int]
  occurs2 is = map fromEnum [elem 2 is, elem 3 is]

  processInput :: String -> [[Int]]
  processInput s = map occurs2 (map count (words s))

  -- proc2 :: [[Int]] -> [Int]
  -- proc2 bb = foldr (\acc cur -> (sum (map head cur)):acc) bb []

  proc3 :: Int -> [[Int]] -> [Int]
  proc3 n is = ((!!) n) >>= is
  
  -- foldr (\acc bs -> ) 0 bb
    
    
    -- length (filter id (map fst bb))
    -- let
    --   m = (map length) . group . sort
    --   n x = length (filter ((==) x) (m input))
    -- in
      -- words
      -- (n 2 + n 3)

  main :: IO ()
  main = interact (\a -> show (processInput a))
