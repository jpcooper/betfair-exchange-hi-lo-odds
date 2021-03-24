{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Betfair.AccountManager.Backtester (Backtester(..),
                                          State(..),
                                          buildState) where

import           Prelude hiding (round)

import           Control.Monad (when)
import qualified Control.Monad.State.Lazy as Lazy (StateT)
import           Control.Monad.State.Lazy (get, gets, modify, put)
import           Data.Map (Map)
import qualified Data.Map as Map (delete, insertWith, lookup, toList)
import qualified Data.Text as Text (pack)

import           Betfair.AccountManager (AccountManager(..), Result)
import           Betfair.Strategy (Account(Account),
                                   Bet(..),
                                   BetOrder(..),
                                   BetAction(..),
                                   BetActionResult(..),
                                   BetId(..),
                                   ResponseBetOrder(..),
                                   ResponseBetOrderErrorType(..),
                                   Side(..),
                                   betfairRound,
                                   calculateLoss,
                                   marketBaseRate)
import           Betfair.Game (Amount(..),
                               Game(marketId,
                                    round,
                                    marketStatus),
                               MarketId,
                               Odds(..),
                               OddsAmount(..),
                               Round,
                               Selection(..),
                               SelectionId,
                               SelectionStatus(..),
                               selections)
import qualified Betfair.Game as MarketStatus (MarketStatus(..))

data State = State {
  betId :: Int,
  lastMarketId :: MarketId,
  lastRound :: Round,
  balance :: Amount,
  bets :: Map (MarketId, SelectionId) [Bet]}

type BacktesterResult m a = Lazy.StateT State m a

data Backtester = Backtester

instance Monad m => AccountManager Backtester m where
  type Result Backtester m a = BacktesterResult m a

  getAccount _ _ = getAccountImpl

  processGame _ _ = processGameImpl

  processBetOrder _ _ = processBetOrderImpl

buildState :: Game -> Amount -> State
buildState game startBalance =
  State {betId = 0,
         lastMarketId = marketId game,
         lastRound = round game,
         balance = startBalance,
         bets = []}

getAccountImpl :: Monad m => BacktesterResult m Account
getAccountImpl = Account <$> gets balance

processGameImpl :: Monad m => Game -> BacktesterResult m ()
processGameImpl game = do
  state <- get
  assertNoBetsFromOldMarket state
  when (shouldSettle state) $ settle game
  modify $ \s -> s {lastMarketId = marketId game, lastRound = round game}

  where isSameMarketId state =
          marketId game == lastMarketId state

        shouldSettle state =
          isSameMarketId state && isSettlingOrSettled

        isSettlingOrSettled =
          (marketStatus game == MarketStatus.SuspendedGameSettling) ||
          (marketStatus game == MarketStatus.Settled)

        assertNoBetsFromOldMarket state =
          when existBetsFromOldMarket $
            error $ "Bets not settled from previous market: " ++ show (bets state)

          where existBetsFromOldMarket =
                  any ((/= marketId game) . fst . fst) $ Map.toList $ bets state

processBetOrderImpl :: Monad m
                    => Game -> BetOrder -> BacktesterResult m ResponseBetOrder
processBetOrderImpl game (BetOrder betMarketId betRound betActions)
  | betMarketId /= marketId game =
      return $ ResponseBetOrderError InvalidMarketId
  | (betRound /= round game) || (marketStatus game /= MarketStatus.Active) =
      return $ ResponseBetOrderError BettingRoundStale
  | otherwise =
      fmap ResponseBetOrder $ mapM processBetAction betActions

  where processBetAction (BetPlace thisBet) =
          bet game thisBet

bet :: Monad m => Game -> Bet -> BacktesterResult m BetActionResult
bet game thisBet@(Bet selectionId side oddsAmount) = do
  state <- get
  if newBalance state >= 0
  then modify updateState >> buildResult
  else return ExposureOrAvailableBalanceExceeded

  where insertBets theseBets =
          Map.insertWith (flip (++)) (marketId game, selectionId) [thisBet] theseBets

        newBalance state =
          thisBalance - loss

          where Amount loss = calculateLoss side oddsAmount
                Amount thisBalance = balance state

        updateState state =
          state {bets = insertBets $ bets state, balance = Amount $ newBalance state}

        buildResult = do
          currentId <- gets betId
          modify (\state -> state { betId = currentId + 1 })
          return $ BetPlacementOkay $ BetId $ Text.pack $ show currentId

settle :: Monad m => Game -> BacktesterResult m ()
settle game = do
  mapM_ settleBets $ selections game

  where settleBets :: Monad m => Selection -> BacktesterResult m ()
        settleBets (Selection selectionId selectionStatus _ _) = do
          state <- get
          if (selectionStatus == Winner) || (selectionStatus == Loser)
          then let key = (marketId game, selectionId)
                   isWinner = selectionStatus == Winner
                   deleteBets s = s {bets = Map.delete key $ bets state}
               in case Map.lookup key $ bets state of
                    Nothing ->
                      return ()

                    Just betsForSelection -> do
                      mapM_ (settleBet state isWinner) betsForSelection
                      modify deleteBets

          else return ()

          where settleBet :: Monad m => State -> Bool -> Bet -> BacktesterResult m ()
                settleBet
                  state
                  isWinner
                  (Bet _ side oddsAmount@(OddsAmount (Odds odds) (Amount amount))) =

                  if newBalance >= 0
                  then put $ state {balance = Amount newBalance}
                  else error "Balance has somehow gone negative."

                  where backLoss = amount
                        layLoss = (odds - 1) * amount

                        profit =
                          case side of
                            Back ->
                              if isWinner
                              then betfairRound $ deductCommission layLoss
                              else negate $ betfairRound amount

                            Lay ->
                              if isWinner
                              then betfairRound $ deductCommission backLoss
                              else negate $ betfairRound layLoss

                        deductCommission a =
                          (1 - marketBaseRate) * a

                        -- Potential loss that was deducted on placing bet
                        Amount deductedLoss =
                          calculateLoss side oddsAmount

                        Amount thisBalance = balance state

                        newBalance =
                          thisBalance + deductedLoss + profit
