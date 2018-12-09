{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified D1P1 as D1P1
import qualified D1P2 as D1P2
import qualified D2P1 as D2P1
import qualified D2P2 as D2P2
import qualified D3P1 as D3P1

main :: IO ()
main = do 
  D3P1.main
  putStrLn "\n"
