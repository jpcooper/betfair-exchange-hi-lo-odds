{-# LANGUAGE OverloadedStrings #-}

module Betfair.Logging (addToLog, logGameResponse, responsesSeparator) where

import           Prelude hiding (appendFile)

import           Data.ByteString.Lazy (ByteString,
                                       append,
                                       appendFile,
                                       fromStrict,
                                       intercalate)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as Strict (ByteString)
import           System.FilePath.Posix ((</>))

import           Betfair.Game (Game, Odds)
import           Betfair.Strategy (ResponseBetOrder)

addToLog :: FilePath
         -> ByteString
         -> Game
         -> [Odds]
         -> String
         -> [ResponseBetOrder]
         -> IO ()
addToLog
  logDirectory
  gameResponse
  game
  odds
  formattedProbabilities
  betOrderRunResults =
  do appendFile (logDirectory </> logFile) logLine

  where logLine =
          intercalate "\n\n"
            ([ gameResponse
             , pack $ show game
             , pack $ show odds
             , pack formattedProbabilities] ++
             map (pack . show) betOrderRunResults ++
             [separator])
        separator = "---\n\n"
        logFile = "log"

logGameResponse :: FilePath
                -> ByteString
                -> IO ()
logGameResponse logDirectory gameResponse =
  appendFile (logDirectory </> responsesFile) $
       append gameResponse $ fromStrict responsesSeparator

  where responsesFile = "responses"

responsesSeparator :: Strict.ByteString
responsesSeparator = "\r\n"