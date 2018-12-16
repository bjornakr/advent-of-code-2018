module D3P1 where
    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import Data.Maybe
    import Data.List.Split

    type Coord = (Int, Int)
    data Claim = Claim Int [Coord] deriving (Show)

    -- Map.unionWith (+)
    count :: Coord -> Map.Map Coord Int -> Map.Map Coord Int
    count c m = 
        let 
            currentCount :: Coord -> Int
            currentCount c = fromMaybe 0 (Map.lookup c m)
        in
            Map.insert c ((currentCount c) + 1) m

    countz :: Claim -> Map.Map Coord Int -> Map.Map Coord Int
    countz (Claim _ coords) m = foldr count m coords
 
    f :: [Claim] -> Map.Map Coord Int
    f = foldr countz Map.empty

    -- g :: Claim -> Map.Map Coord Int
    -- g cs = foldr count Map.empty cs

    -- h :: [[Coord]] -> Map.Map Coord Int
    -- h cs = Map.unionWith (+) (map g cs)

    solve :: Map.Map Coord Int -> Int
    solve m = length (Map.filter (\a -> a > 1) m)

    toCoords :: (Int, Int, Int, Int, Int) -> Claim
    toCoords (id0, x, y, w, l) = Claim id0 [(i,j) | i <- [x..(x+(w-1))], j <- [y..(y+(l-1))]]
    
    parseSpec :: String -> (Int, Int, Int, Int, Int)
    parseSpec line = 
        let
            parts = splitOneOf " #@,:x" line
        in
            (read $ parts !! 1, read $ parts !! 4, read $ parts !! 5, read $ parts !! 7, read $ parts !! 8)

    main =  interact (show . solve . f . (map toCoords) . (map parseSpec) . lines)
    main2 = interact (show . ff . (map toCoords) . (map parseSpec) . lines)

    hasOverlap c1 c2 = (Set.intersection (Set.fromList c1) (Set.fromList c2)) /= Set.empty

    ff :: [Claim] -> Claim
    
    ff cls@(c0:_) = let
        board = (Map.filter (\a -> a > 1) (f cls))

        loop [] = error "No claim found"
        loop (z:zs) =            
            if (Map.filter (\a -> a > 1) (countz z board)) == board
                then z
                else loop zs
        in
            loop cls
