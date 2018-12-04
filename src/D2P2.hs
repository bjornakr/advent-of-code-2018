module D2P2 where
  import Data.List

  isMatch _ _ 2 = False
  isMatch [] _ _ = True
  isMatch (a:as) (b:bs) strike =
    if (a == b) 
      then isMatch as bs strike
      else isMatch as bs (strike + 1)

  matchFirstWithRest (_:[]) = Nothing
  matchFirstWithRest (id0:id1:ids) =
    if isMatch id0 id1 0  -- take 0 away
      then Just (id0, id1)  --fix
      else matchFirstWithRest (id0:ids)

  checkAllIds [] = "No matching ids :'("
  checkAllIds ids@(_:rest) = 
    case matchFirstWithRest ids of
      Nothing -> checkAllIds rest
      Just (id0, id1) -> intersect id0 id1

  main = interact (show . checkAllIds . words)
