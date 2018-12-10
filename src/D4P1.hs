module D4P1 where
  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO
  import qualified Data.Attoparsec.Text as AP
  import Control.Applicative

  -- data GuardId = GuardId Int
  data EventAction = Sleep | WakeUp | BeginShift Int deriving (Show)
  data Time = Time T.Text Int deriving (Show)
  data Event = Event Time EventAction deriving (Show)


  -- [1518-09-17 23:48] Guard #1307 begins shift

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

  main = do
    input <- TIO.readFile "src/d4.txt"
    -- let event = (AP.parseOnly eventParser) (T.pack "[1518-09-17 23:48] Guard #1307 begins shift")
    -- putStrLn (show event)
    let events = fmap (AP.parseOnly eventParser) (T.lines input)
    putStrLn $ (show events)