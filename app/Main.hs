module Main where

import Lib
import qualified D1P1 as D1P1
import qualified D1P2 as D1P2
import qualified D2P1 as D2P1
import qualified D2P2 as D2P2

main :: IO ()
main = do 
  D2P2.main
  putStrLn "\n"
