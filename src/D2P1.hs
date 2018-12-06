module D2P1 where
  import Data.List

  countOccurances :: String -> [Int]
  countOccurances = (map length) . group . sort

  ofTwoAndThree :: [Int] -> [Int]
  ofTwoAndThree is = map fromEnum [elem 2 is, elem 3 is]

  occurances :: String -> [[Int]]
  occurances = (map (ofTwoAndThree . countOccurances)) . words

  calculateProduct :: [[Int]] -> Int
  calculateProduct is = sumColumn 0 is * sumColumn 1 is

  sumColumn :: Int -> [[Int]] -> Int
  sumColumn n is = sum (map ((flip (!!)) n) is)

  main :: IO ()
  main = do
    interact (show . calculateProduct . occurances)
