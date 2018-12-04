module D1P2 where
  import qualified Data.IntSet as IntSet

  cycleInput :: String -> [Int]
  cycleInput = cycle . (map read) . (map (filter ((/=) '+'))) . words


  --scanl
  findDuplicate :: [Int] -> Int
  findDuplicate is = 
    let 
      f :: [Int] -> IntSet.IntSet -> Int -> Int
      f (i:is) ys sum =
        if IntSet.member sum ys
          then sum
          else f is (IntSet.insert sum ys) (sum+i)
    in
      f is IntSet.empty 0

  main :: IO ()
  main = interact (show . findDuplicate . cycleInput)