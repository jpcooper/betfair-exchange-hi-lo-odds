module Betfair.Strategy.Dumb (Configuration(..),
                              State,
                              dumb,
                              emptyState) where

import Prelude hiding (round)

import Control.Monad (when)
import Control.Monad.State.Lazy (get, put)

import Betfair.Strategy (Account(balance),
                         GameRepresentation(..),
                         Strategy,
                         Side(..),
                         Bet(..),
                         BetAction(..),
                         BetOrder(..),
                         betfairRound,
                         calculateLayLoss,
                         getMaxProfitableLayOdds)
import Betfair.Game (Amount(Amount, getAmount),
                     MarketId,
                     Round,
                     Odds(Odds),
                     OddsAmount(OddsAmount),
                     MarketStatus(Active))

data Configuration = Configuration {
  maxLoss :: Amount,
  minBet :: Amount}

data State = State (Maybe (Round, MarketId))
  deriving Eq

emptyState :: State
emptyState = State Nothing

dumb :: Monad m => Configuration -> Strategy State m
dumb configuration account game odds = do
  state <- get
  if shouldBet state
  then strategy
  else doNothing

  where newState = State $ Just (round game, marketId game)

        shouldBet state =
          not haveAlreadyBetInRound && (marketStatus game == Active)

          where haveAlreadyBetInRound = state == newState

        doNothing =
          return []

        makeBet (selectionId, realOdds) =
          if (amount >= getAmount (minBet configuration)) &&
             (projectedLoss <= thisBalance)
          then [BetOrder (marketId game) (round game) [BetPlace bet]]
          else []

          where Amount l =
                  maxLoss configuration

                Odds maxProfitableLayOdds =
                  getMaxProfitableLayOdds (Amount l) realOdds

                amount =
                  roundDown (l / (maxProfitableLayOdds - 1))

                oddsAmount =
                  OddsAmount (Odds maxProfitableLayOdds) (Amount amount)

                projectedLoss =
                  betfairRound $ getAmount $ calculateLayLoss oddsAmount

                bet =
                  Bet selectionId Lay oddsAmount

                Amount thisBalance =
                  balance account

        bets = take 1 $
                 concatMap makeBet $
                   zip (inPlaySelections game) odds
        strategy = do
          when (length bets > 0) $
            put newState
          return bets

-- Round down to nearest cent
roundDown :: Rational -> Rational
roundDown value =
  fromIntegral cents / decimalFactor

  where decimalFactor = 100 :: Rational
        cents = floor (value * decimalFactor) :: Int
