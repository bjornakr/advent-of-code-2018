module D4P1 where
  import Data.Either
  import Data.List
  import Data.Ord
  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO
  import qualified Data.Text.Read as TR
  import qualified Data.Attoparsec.Text as AP
  import Control.Applicative
  import Text.Printf (printf)

  sleepParser :: AP.Parser EventAction
  sleepParser = do
    AP.string (T.pack "falls asleep")
    pure Sleep
  
  wakeUpParser :: AP.Parser EventAction
  wakeUpParser = do
    AP.string (T.pack "wakes up")
    pure WakeUp

  beginShiftParser :: AP.Parser EventAction
  beginShiftParser = do
    AP.string (T.pack "Guard #")
    guardId <- AP.decimal
    pure $ BeginShift guardId

  minutesParser :: AP.Parser Int
  minutesParser = do
    AP.decimal
    AP.char ':'
    minutes <- AP.decimal
    pure $ minutes

  eventParser :: AP.Parser Event
  eventParser = do
    AP.char '['
    date0 <- AP.take 10
    AP.char ' '
    minutes <- minutesParser
    AP.char ']'
    AP.char ' '
    action <- beginShiftParser <|> sleepParser <|> wakeUpParser
    pure $ Event (Time date0
     minutes) action

  -- data GuardId = GuardId Int deriving (Show, Eq)
  type GuardId = Int
  data EventAction = Sleep | WakeUp | BeginShift GuardId deriving (Show, Eq)
  data Time = Time T.Text Int deriving (Show, Eq)
  data Event = Event Time EventAction deriving (Show, Eq)

  -- SF: Finnes denne fra før?
  tshow :: (Show a) => a -> T.Text
  tshow = T.pack . show

  instance Ord Event where
    Event (Time date1 minutes1) _ `compare` Event (Time date2 minutes2) _ =
      compare 
        (date1 `T.append` (T.pack(printf "%02d" minutes1))) 
        (date2 `T.append` (T.pack(printf "%02d" minutes2)))

  withGuardId :: [Event] -> [(GuardId, Event)]
  withGuardId es =
    let 
      worker :: [Event] -> Maybe GuardId -> [(GuardId, Event)]
      worker [] _ = []
      worker (e@(Event _ (BeginShift guardId)):es) _ = (guardId, e) : (worker es (Just guardId))
      worker (e:es) gid@(Just guardId) = (guardId, e) : (worker es gid)
      worker (e:es) Nothing = worker es Nothing
    in
      worker es Nothing
      
  groupByGuardId :: [(GuardId, Event)] -> [[(GuardId, Event)]]
  groupByGuardId = groupBy (\e1 e2 -> (fst e1) == (fst e2)) . sort
  
  sumSleep :: [Event] -> Int
  sumSleep es =
    let 
      worker [] acc _ = acc
      worker ((Event (Time _ minutes) Sleep):es) acc start =
        worker es acc minutes
      worker ((Event (Time _ minutes) WakeUp):es) acc start =
        worker es (acc + (minutes - start)) 0
      worker (e:es) acc start =
        worker es acc start 
    in
      worker es 0 0
    
  -- bestMinute :: [Event] -> Int
  bestMinute [] = 0
  bestMinute es =
    let
      worker [] _ acc = [0]:acc
      worker ((Event (Time _ minutes) Sleep):es) start acc =
        worker es minutes acc
      worker ((Event (Time _ minutes) WakeUp):es) start acc =
        worker es 0 ([start..(minutes -1)]:acc)
      worker (e:es) start acc =
        worker es start acc
    
      findMostOccured = head . snd . maximum . (map (\l -> (length l, l))) . group . sort
    in
      (findMostOccured . concat) (worker es 0 [])

  bestMinute2 es =
    let
      worker [] _ acc = [0]:acc
      worker ((Event (Time _ minutes) Sleep):es) start acc =
        worker es minutes acc
      worker ((Event (Time _ minutes) WakeUp):es) start acc =
        worker es 0 ([start..(minutes -1)]:acc)
      worker (e:es) start acc =
        worker es start acc
    
      findMostOccured = maximum . (map (\l -> (length l, l))) . group . sort
    in
      (findMostOccured . concat) (worker es 0 [])
  -- e:(takeWhile (\(gid2, _) -> gid == gid2) es))

  getEvent = map (\(guardId, event) -> event)
  
  -- solve3 :: [(GuardId, Event)] -> (GuardId, Int)
  solve3 events@((guardId, event):es) = (guardId, (bestMinute . getEvent) events)

  solve2 :: [[(GuardId, Event)]] -> [(GuardId, Event)]
  -- (sum, (guardId, event))
  solve2 = snd . maximum . (map (\ges -> (sumSleep (getEvent ges), ges)))
  
  solve1 :: [Event] -> [[(GuardId, Event)]]
  solve1 = groupByGuardId . withGuardId . sort

  -- solve4 :: [[(GuardId, Event)]] -> (Int, [(GuardId, Event)])
  solve4 = maximum . (map (\ges -> (bestMinute2 (getEvent ges), ges)))

  main = do
    input <- TIO.readFile "src/d4.txt"
    -- let event = (AP.parseOnly eventParser) (T.pack "[1518-09-17 23:48] Guard #1307 begins shift")
    -- putStrLn (show event)
    let xes = fromRight [] $ traverse (AP.parseOnly eventParser) (T.lines input)
    -- (putStrLn . show) $ (solve3 . solve2 . solve1) xes
    (putStrLn . show) $ (solve4 . solve1) xes
    putStrLn ""

    -- solute