{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude (IO,
                          FilePath,
                          Maybe(..),
                          String,
                          (>>=),
                          (<),
                          (||),
                          error,
                          fromIntegral)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, return)
import           Control.Monad.IO.Class (liftIO)
import           Data.Function (($))
import           Data.List (length, maximum, map, lines)
import           UI.NCurses
  ( Curses
  , Update
  , CursorMode(CursorInvisible)
  , clear
  , defaultWindow
  , runCurses
  , windowSize
  , moveCursor
  , updateWindow
  , render
  , runCurses
  , setCursorMode
  )
import qualified UI.NCurses as NCurses (drawString)
import           System.Environment (getArgs)
import           System.Directory (doesDirectoryExist)

import           Betfair.Formatting (formatGameAndOdds)
import           Betfair.IO (getGame)
import           Betfair.Logging (addToLog)
import           Betfair.Model.Game.Parsing (parseGame)
import           Betfair.Wrapper (getOdds)

gameUrl :: String
gameUrl = "https://api.games.betfair.com/rest/v1/channels/1444093/snapshot?selectionsType=CorrectPredictions"

main :: IO ()
main = do
  maybeLogDirectory <- getLogDirectory
  runCurses $ do
    _ <- setCursorMode CursorInvisible
    forever $ do
      displayOdds maybeLogDirectory
      liftIO $ threadDelay 1000000

getLogDirectory :: IO (Maybe String)
getLogDirectory = getArgs >>= handle
  where handle [logDirectory] = do
          doesDirectoryExistResult <- doesDirectoryExist logDirectory
          if doesDirectoryExistResult
            then return $ Just logDirectory
            else error "Given log directory does not exist."
        handle _ =
          return Nothing

displayOdds :: Maybe FilePath -> Curses ()
displayOdds maybeLogDirectory =
  do gameResponse <- liftIO $ getGame gameUrl
     let game = parseGame gameResponse
     stringToDraw <- liftIO $ getStringToDraw gameResponse game
     let update = drawString stringToDraw
     window <- defaultWindow
     _ <- updateWindow window update
     render

  where getStringToDraw gameResponse game =
          do odds <- liftIO $ getOdds game
             let formattedOdds = formatGameAndOdds game odds

             case maybeLogDirectory of
               Just logDirectory ->
                 liftIO $ addToLog
                  logDirectory
                  gameResponse
                  game
                  odds
                  formattedOdds

               Nothing ->
                 return ()

             return formattedOdds

drawString :: String -> Update ()
drawString string =
  do clear
     moveCursor 0 0
     _ <- resizeToStringSize
     NCurses.drawString string
     return ()

  where linesInString = lines string
        stringWidth = maximum $ map length linesInString
        numberLines = length linesInString
        resizeToStringSize =
          do (windowLength, windowWidth) <- windowSize

             if (windowLength < fromIntegral numberLines) ||
                (windowWidth < fromIntegral stringWidth)
               then error "The window isn't large enough to draw the output. Try making it a bit bigger."
               else return ()
