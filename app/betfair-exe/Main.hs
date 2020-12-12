{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude (Bool(False),
                          IO,
                          FilePath,
                          Integer,
                          String,
                          (++),
                          (>>=),
                          (<),
                          (||),
                          (-),
                          div,
                          error,
                          max,
                          read,
                          fromIntegral,
                          show)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, mapM, return)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (StateT, lift, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
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
import           Network.HTTP.Client (ManagerSettings)
import           System.CPUTime (getCPUTime)
import           System.Environment (getArgs)
import           System.Directory (doesDirectoryExist)

import           Betfair.Controller.Controller (Command(BetActions))
import           Betfair.Controller.Dumb (DumbState, dumb, emptyDumbState)
import           Betfair.Formatting (formatCommands, formatGameAndOdds)
import           Betfair.IO (Login(Login),
                             buildManagerSettings,
                             getGame,
                             postOrder)
import           Betfair.Logging (addToLog)
import           Betfair.Model.Game (Game)
import           Betfair.Model.Game.Parsing (parseGame, formatCommand)
import           Betfair.Wrapper (getOdds)

picosecondsInMilliseconds :: Integer
picosecondsInMilliseconds = 1000000000

picosecondsInNanoseconds :: Integer
picosecondsInNanoseconds = 1000

main :: IO ()
main = do
  runCurses $ do
    _ <- setCursorMode CursorInvisible
    evalStateT run emptyDumbState

  where run = do
          (logDirectory, login, useProxy) <- liftIO getConfiguration
          forever $ do
            (displayDuration, _) <- timePicoseconds $
              displayOdds logDirectory login $ buildManagerSettings useProxy
            let displayDurationInNanoseconds =
                  div displayDuration picosecondsInNanoseconds
            liftIO $
              threadDelay $
              max 0 (1000000 - fromIntegral displayDurationInNanoseconds)

timePicoseconds :: MonadIO m => m a -> m (Integer, a)
timePicoseconds action = do
  start <- liftIO $ getCPUTime
  result <- action
  end <- liftIO $ getCPUTime
  return (end - start, result)

getConfiguration :: IO (FilePath, Login, Bool)
getConfiguration = getArgs >>= handle
  where handle [logDirectory, username, password, useProxy] = do
          doesDirectoryExistResult <- doesDirectoryExist logDirectory
          if doesDirectoryExistResult
          then return $ (logDirectory, Login username password, read useProxy)
          else error "Given log directory does not exist."
        handle _ =
          error "Incorrect number of command line arguments."

runCommands :: Login -> ManagerSettings -> Game -> [Command] -> IO [Lazy.ByteString]
runCommands login managerSettings game commands =
  mapM executeCommand commands

  where executeCommand command@(BetActions _) =
          postOrder managerSettings login $ formatCommand isFormatPretty game command

          where isFormatPretty = False

displayOdds :: FilePath -> Login -> ManagerSettings -> StateT DumbState Curses ()
displayOdds logDirectory login managerSettings =
  do (getResponseDuration, gameResponse) <-
       liftIO $ timePicoseconds $ getGame managerSettings
     let game = parseGame gameResponse
     odds <- liftIO $ getOdds game
     commands <- dumb game odds
     commandRunResults <- liftIO $ runCommands login managerSettings game commands
     stringToDraw <- liftIO $
       getStringToDraw
         gameResponse
         game
         odds
         commands
         commandRunResults
         getResponseDuration
     let update = drawString stringToDraw
     window <- lift defaultWindow
     _ <- lift $ updateWindow window update
     lift render

  where getStringToDraw
          gameResponse
          game
          odds
          commands
          commandRunResults
          getResponseDuration =

          do let formattedOdds = formatGameAndOdds game odds
             let formattedCommands = formatCommands game commands
             let formattedGetResponseDuration =
                   show (div getResponseDuration picosecondsInMilliseconds)
             let stringToDraw =
                   formattedOdds ++ "\n\n" ++
                   formattedCommands ++ "\n\n" ++
                   formattedGetResponseDuration
             liftIO $ addToLog
               logDirectory
               gameResponse
               game
               odds
               stringToDraw
               commandRunResults

             return stringToDraw

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
