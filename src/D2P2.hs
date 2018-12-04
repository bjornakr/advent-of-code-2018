module D2P2 where
  import Data.List

  isMatch _ _ 2 = False
  isMatch [] _ _ = True
  isMatch (a:as) (b:bs) strike =
    if (a == b) 
      then isMatch as bs strike
      else isMatch as bs (strike + 1)

  matchHeadWithRest (_:[]) = Nothing
  matchHeadWithRest (id0:id1:rest) =
    if isMatch id0 id1 0  -- take 0 away
      then Just (id0, id1)
      else matchHeadWithRest (id0:rest)

  checkAllIds [] = "No matching ids :'("
  checkAllIds ids@(_:rest) = 
    case matchHeadWithRest ids of
      Nothing -> checkAllIds rest
      Just (id0, id1) -> intersect id0 id1

  main = interact (checkAllIds . words)
