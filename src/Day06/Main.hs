module Day06.Main where
  import Data.List.Split
  import Data.List
  -- import Data.Array
  import qualified Data.Map as Map

  type TargetId = Int
  type Distance = Int
  type Coord = (Int, Int)
  data Cell = NotDetermined | Target TargetId | BelongsTo TargetId Distance | Intermediate
  -- data Edge = NW Coord | NE Coord | SW Coord | Semigroup
  -- data Edges = Edges Coord Coord Coord Coord
  type Grid = Map.Map Coord Cell

  parseCoord :: String -> (Int, Int)
  parseCoord s = 
    let
      list = (map read (splitOn ", " s))
    in
      (list !! 0, list !! 1)
    
  getDistance :: (Ord a, Num a) => [a] -> a
  getDistance as = (maximum as) - (minimum as)
  
  getHeight :: [Coord] -> Int
  getHeight coords = getDistance (map fst coords)
 
  getWidth :: [Coord] -> Int
  getWidth coords = getDistance (map snd coords)

  initializeGrid :: [Coord] -> Map.Map Coord Cell
  initializeGrid coords = 
    let 
      allCoords = do
        x <- [0..(getWidth coords)]
        y <- [0..(getHeight coords)]
        pure (x, y)
    in
      Map.fromList $ map (\c -> (c, NotDetermined)) allCoords -- SF: må man skrive c eksplisitt?

  placeTargets :: [Coord] -> Grid -> Grid
  -- placeTargets [] _ g = g
  -- placeTargets (c:cs) (tid:tids) g =
  --   placeTargets cs tids (Map.insert c (Target tid) g)
  placeTargets coords grid =
    let targetIds = [1..] in
    foldr 
      (\(c, tid) g -> Map.insert c (Target tid) g) 
      grid 
      (zip coords targetIds)


  claim :: Coord -> TargetId -> Distance -> Grid -> Maybe Grid
  claim coord tid dist grid = let
    newCell = case (Map.lookup coord grid) of
      Just NotDetermined -> Just (BelongsTo tid dist)
      Just (BelongsTo tid2 _) -> if (tid == tid2) then Nothing else Just (Intermediate)
      Just (Target tid2) -> error $ (show tid) ++ "crashed into " ++ (show tid2)
    in
      fmap (\nc -> Map.insert coord nc grid) newCell

  moveNorth (x, y) = (x, y-1)
  moveSouth (x, y) = (x, y+1)
  moveWest (x, y) = (x-1, y)
  moveEast (x, y) = (x+1, y)
  moves = [moveNorth, moveSouth, moveWest, moveEast]

  getTargetId :: Cell -> Maybe (TargetId, Distance)
  getTargetId c = case c of
    Target tid -> Just (tid, 0)
    BelongsTo tid dist -> Just (tid, distance)
    _ -> Nothing

  expand :: Coord -> Grid -> Grid
  expand coord grid = 
    case (Map.lookup coord grid) >>= getTargetId of
      Nothing -> grid
      Just (tid, dist) -> map (\move -> claim (move coord) tid dist grid) moves -- må folde


  main = do
    input <- readFile "src/Day06/input.txt"
    let coords = sort (map parseCoord (lines input))
    (putStrLn . show) coords
    putStrLn "!"