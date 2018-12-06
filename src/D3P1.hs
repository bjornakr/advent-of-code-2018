module D3P1 where
    -- import qualified Data.Set as Set
    import qualified Data.Map as Map
    import Data.Maybe
    import Data.List.Split

    type Coord = (Int, Int)


    -- Map.unionWith (+)
    count :: Coord -> Map.Map Coord Int -> Map.Map Coord Int
    count c m = 
        let 
            currentCount :: Coord -> Int
            currentCount c = fromMaybe 0 (Map.lookup c m)
        in
            Map.insert c ((currentCount c) + 1) m

    countz :: [Coord] -> Map.Map Coord Int -> Map.Map Coord Int
    countz cs m = foldr count m cs
 
    f :: [[Coord]] -> Map.Map Coord Int
    f = foldr countz Map.empty

    g :: [Coord] -> Map.Map Coord Int
    g cs = foldr count Map.empty cs

    -- h :: [[Coord]] -> Map.Map Coord Int
    -- h cs = Map.unionWith (+) (map g cs)

    solve :: Map.Map Coord Int -> Int
    solve m = length (Map.filter (\a -> a > 1) m)

    toCoords :: (Int, Int, Int, Int) -> [Coord]
    toCoords (x, y, w, l) = [(i,j) | i <- [x..(x+(w-1))], j <- [y..(y+(l-1))]]
    
    parseSpec :: String -> (Int, Int, Int, Int)
    parseSpec line = 
        let
            parts = splitOneOf " #@,:x" line
        in
            (read $ parts !! 4, read $ parts !! 5, read $ parts !! 7, read $ parts !! 8)

    main =  interact (show . solve . f . (map toCoords) . (map parseSpec) . lines)
    