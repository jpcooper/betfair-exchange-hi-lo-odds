{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, print, putStrLn)

import           Control.Monad.State.Lazy (evalState)
import           Data.ByteString.Lazy (getContents)

import           Betfair.App.Common (maxLoss)
import           Betfair.App.Common.Formatting (formatGameAndOdds, formatCommands)
import           Betfair.App.Common.Wrapper (getOdds)
import           Betfair.Controller.Dumb (emptyDumbState, dumb)
import qualified Betfair.Controller.Dumb as Dumb (Configuration(Configuration))
import           Betfair.Model.Game.Parsing (parseGame)

main :: IO ()
main = do
  input <- getContents
  let game = parseGame input
  odds <- getOdds game
  let dumbConfiguration = Dumb.Configuration maxLoss
  let commands = evalState (dumb dumbConfiguration game odds) emptyDumbState
  let formattedGameAndOdds = formatGameAndOdds maxLoss game odds
  let formattedCommands = formatCommands game commands
  print odds
  putStrLn formattedGameAndOdds
  putStrLn ""
  putStrLn formattedCommands
