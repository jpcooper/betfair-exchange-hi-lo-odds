{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prelude hiding (getContents)

import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Lazy (StateT, gets, lift, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.ByteString.Lazy (getContents)
import           Data.ByteString.Lazy.Search (split)
import           System.CPUTime (getCPUTime)
import           System.Environment (getArgs)
import           System.Directory (doesDirectoryExist)
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

import           Betfair.AccountManager
import qualified Betfair.AccountManager.Betfair as AccountManager (Betfair(..))
import           Betfair.App.Common (maxLoss, minBet)
import           Betfair.App.Common.Formatting (formatBetOrders, formatGameAndOdds)
import           Betfair.App.Common.Wrapper (getOdds)
import           Betfair.Serialisation (formatRational)
import           Betfair.Strategy (Account(balance),
                                   buildGameRepresentation)
import           Betfair.Strategy.Dumb (dumb)
import qualified Betfair.Strategy.Dumb as Dumb (Configuration(..),
                                                State,
                                                emptyState)
import           Betfair.IO (Login,
                             ProxyChoice,
                             buildManagerSettings,
                             getGame)
import           Betfair.Logging (addToLog, logGameResponse, responsesSeparator)
import           Betfair.Game (Amount(..))
import           Betfair.Game.Serialisation (parseGame)

picosecondsInMilliseconds :: Integer
picosecondsInMilliseconds = 1000000000

picosecondsInMicroseconds :: Integer
picosecondsInMicroseconds = 1000000

dumbConfiguration :: Dumb.Configuration
dumbConfiguration = Dumb.Configuration maxLoss minBet

startBalance :: Amount
startBalance = Amount 100

data State = State {
  _dumbState :: Dumb.State,
  -- _backtesterState :: Backtester.State,
  _account :: Account}

makeLenses ''State

data SourceChoice = Internet Login ProxyChoice
                  | FilesFromStdin
  deriving Read

main :: IO ()
main = do
  (logDirectory, sourceChoice) <- getConfiguration
  gameList <- getGameList sourceChoice
  let thisAccountManager = accountManager sourceChoice
  runCurses $ do
    _ <- setCursorMode CursorInvisible
    startAccount <- getAccount Proxy thisAccountManager
    evalStateT (run logDirectory gameList thisAccountManager) $ buildState startAccount

  where run logDirectory gameList thisAccountManager =
          forM_ gameList $ \game -> do
            throttle $ displayOdds logDirectory thisAccountManager game

        accountManager sourceChoice = case sourceChoice of
          Internet login proxyChoice ->
            AccountManager.Betfair (buildManagerSettings proxyChoice) login

          _ ->
            error "Internet source only."

throttle :: MonadIO m => m () -> m ()
throttle action = do
  (runDuration, _) <- timePicoseconds action
  let runDurationInMicroseconds = div runDuration picosecondsInMicroseconds
  liftIO $ threadDelay $
    max 0 (1000000 - fromIntegral runDurationInMicroseconds)

getGameList :: SourceChoice -> IO [IO Lazy.ByteString]
getGameList sourceChoice = case sourceChoice of
  Internet _ proxyChoice ->
    internetGameList proxyChoice

  FilesFromStdin ->
    stdinGameList

  where internetGameList proxyChoice =
          return $ repeat $ getGame managerSettings

          where managerSettings = buildManagerSettings proxyChoice

        stdinGameList =
          map return <$> split responsesSeparator <$> getContents

buildState :: Account -> State
buildState thisAccount =
  State {_dumbState = Dumb.emptyState,
         _account = thisAccount}

timePicoseconds :: MonadIO m => m a -> m (Integer, a)
timePicoseconds action = do
  start <- liftIO $ getCPUTime
  result <- action
  end <- liftIO $ getCPUTime
  return (end - start, result)

getConfiguration :: IO (FilePath, SourceChoice)
getConfiguration = getArgs >>= handle
  where handle [logDirectory, sourceChoice] = do
          doesDirectoryExistResult <- doesDirectoryExist logDirectory
          if doesDirectoryExistResult
          then return $ (logDirectory, read sourceChoice)
          else error "Given log directory does not exist."
        handle _ =
          error "Incorrect number of command line arguments."

displayOdds :: FilePath
            -> AccountManager.Betfair
            -> IO Lazy.ByteString
            -> StateT State Curses ()
displayOdds logDirectory accountManager gameAction =
  do (getResponseDuration, gameResponse) <-
       liftIO $ timePicoseconds $ gameAction
     liftIO $ logGameResponse logDirectory gameResponse
     let game = parseGame gameResponse
     liftIO $ processGame Proxy accountManager game
     odds <- liftIO $ getOdds game
     thisAccount <- use account
     let gameRepresentation = buildGameRepresentation game
     betOrders <-
       zoom dumbState $ dumb dumbConfiguration thisAccount gameRepresentation odds
     betOrderRunResults <- mapM (processBetOrder Proxy accountManager game) betOrders
     account <~ getAccount Proxy accountManager
     currentBalance <- gets (getAmount . balance . _account)

     let stringToDraw =
           getStringToDraw
             game
             odds
             betOrders
             getResponseDuration
             currentBalance

     liftIO $ addToLog
           logDirectory
           gameResponse
           game
           odds
           stringToDraw
           betOrderRunResults

     updateWithString stringToDraw

  where getStringToDraw
          game
          odds
          betOrders
          getResponseDuration
          currentBalance = do

          let formattedOdds = formatGameAndOdds maxLoss game odds
          let formattedBetOrders = formatBetOrders betOrders
          let formattedGetResponseDuration =
                show (div getResponseDuration picosecondsInMilliseconds)
          let formattedBalance = formatRational currentBalance 2
          let stringToDraw =
                formattedOdds ++ "\n\n" ++
                formattedBetOrders ++ "\n\n" ++
                formattedGetResponseDuration ++ "    " ++ formattedBalance

          stringToDraw

        updateWithString stringToDraw = do
          let update = drawString stringToDraw
          window <- lift defaultWindow
          _ <- lift $ updateWindow window update
          lift render

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
