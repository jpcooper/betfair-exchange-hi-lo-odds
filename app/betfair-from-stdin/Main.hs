{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, print, putStrLn)

import Control.Monad.State.Lazy (evalState)
import Data.ByteString.Lazy (getContents)

import Betfair.Controller.Dumb (emptyDumbState, dumb)
import Betfair.Model.Game.Parsing (parseGame)
import Betfair.Wrapper (getOdds)
import Betfair.Formatting (formatGameAndOdds, formatCommands)

main :: IO ()
main = do
  input <- getContents
  let game = parseGame input
  odds <- getOdds game
  let commands = evalState (dumb game odds) emptyDumbState
  let formattedGameAndOdds = formatGameAndOdds game odds
  let formattedCommands = formatCommands game commands
  print odds
  putStrLn formattedGameAndOdds
  putStrLn ""
  putStrLn formattedCommands
