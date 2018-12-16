module Day05.Main where
  import Data.Char

  reactsWith :: Char -> Char -> Bool
  reactsWith c1 c2 =
    c1 /= c2 && (((toUpper c1) == c2) || ((toUpper c2) == c1))

  process :: String -> String
  process s = 
    let
      worker :: String -> String -> String
      worker [] acc = acc
      worker (s:ss) [] = worker ss [s]
      worker (s:ss) (a:acc) =
        if s `reactsWith` a
          then worker ss acc
          else worker ss (s:a:acc)
    in
      worker s [] 

  switchPolarity c = 
    if (isUpper c)
      then toLower c
      else toUpper c

  removePolymer :: Char -> String -> String
  removePolymer c = (filter ((/=) (switchPolarity c))) . (filter ((/=) c))

  rpols = map (removePolymer) ['a'..'z']

  solve s = do
    let all = (map (\r -> r s) rpols)
    let procd = map process all
    let res = map length procd
    -- procd
    minimum res


  main = do
    input <- readFile "src/Day05/input.txt"
    let result = process input
    let r2 = solve input
    (putStrLn . show) (length result)
    (putStrLn . show) r2
    putStrLn "!"