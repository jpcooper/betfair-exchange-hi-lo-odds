module Main where

import Prelude (IO, print, putStrLn)

import Data.ByteString.Lazy (getContents)

import Betfair.Model.Game.Parsing (parseGame)
import Betfair.Wrapper (getOdds)
import Betfair.Formatting (formatGameAndOdds)

main :: IO ()
main = do
  input <- getContents
  let game = parseGame input
  odds <- getOdds game
  let formatted = formatGameAndOdds game odds
  print odds
  putStrLn formatted
