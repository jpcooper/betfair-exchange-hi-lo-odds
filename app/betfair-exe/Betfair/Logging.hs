{-# LANGUAGE OverloadedStrings #-}

module Betfair.Logging (addToLog) where

import           Prelude (FilePath, String, IO, show)

import           Data.ByteString.Lazy (ByteString, appendFile, writeFile)
import qualified Data.ByteString.Lazy as ByteString (intercalate)
import           Data.Function (($))
import qualified Data.List as List (intercalate)
import           Data.String (IsString(fromString))
import           Data.Time (getCurrentTime)
import           System.FilePath.Posix ((</>))
import           System.IO.Temp (emptyTempFile)

import           Betfair.Model.Game (Game, Odds)

addToLog :: FilePath -> ByteString -> Game -> [Odds] -> String -> IO ()
addToLog logDirectory gameResponse game odds formattedProbabilities =
  do responsePath <- getResponsePath
     writeFile responsePath gameResponse
     appendFile (logDirectory </> logFile) $ logLine responsePath

  where logLine responsePath =
          ByteString.intercalate "\n\n"
            [ fromString responsePath
            , gameResponse
            , fromString $ show game
            , fromString $ show odds
            , fromString formattedProbabilities
            , separator
            ]
        separator = "---\n\n"
        logFile = "log"
        getResponsePath =
          do time <- getCurrentTime
             let fileTemplate = List.intercalate "-" ["response", show time]
             emptyTempFile logDirectory fileTemplate
