{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding (getContents)

import           Control.Monad.State.Lazy (evalState)
import           Data.ByteString.Lazy (getContents)

import           Betfair.App.Common (maxLoss, minBet)
import           Betfair.App.Common.Formatting (formatGameAndOdds, formatBetOrders)
import           Betfair.App.Common.Wrapper (getOdds)
import           Betfair.Strategy.Dumb (dumb)
import qualified Betfair.Strategy.Dumb as Dumb (Configuration(Configuration),
                                                emptyState)
import           Betfair.Game (Amount(Amount))
import           Betfair.Game.Serialisation (parseGame)
import           Betfair.Strategy (Account(Account),
                                   buildGameRepresentation)

startAccount :: Account
startAccount = Account $ Amount 100

main :: IO ()
main = do
  input <- getContents
  let game = parseGame input
  odds <- getOdds game
  let dumbConfiguration = Dumb.Configuration maxLoss minBet
  let gameRepresentation = buildGameRepresentation game
  let betOrders =
        evalState
          (dumb dumbConfiguration startAccount gameRepresentation odds)
          Dumb.emptyState
  let formattedGameAndOdds = formatGameAndOdds maxLoss game odds
  let formattedBetOrders = formatBetOrders betOrders
  print odds
  putStrLn formattedGameAndOdds
  putStrLn ""
  putStrLn formattedBetOrders
