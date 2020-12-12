module Betfair.Controller.Dumb (DumbState, dumb, emptyDumbState) where

import Prelude (Eq,
                Int,
                Monad,
                Rational,
                (==),
                (/=),
                (>),
                (<),
                (>=),
                -- (<=),
                (&&),
                (+),
                (-),
                (*),
                (/),
                ($),
                concatMap,
                error,
                fmap,
                floor,
                fromIntegral,
                length,
                negate,
                product,
                recip,
                rem,
                return,
                take)

import Control.Monad (when)
import Control.Monad.State.Lazy (get, put)
import Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import Data.Vector (toList)

import Betfair.Controller.Common (calculateCheapestOdds,
                                  calculateMargin,
                                  zipSelectionsAndOdds)
import Betfair.Controller.Controller (Controller,
                                      Side(..),
                                      BetAction(..),
                                      Command(..))
import Betfair.Model.Game (Amount(Amount),
                           Game(marketId,
                                marketStatus,
                                round,
                                selections),
                           MarketId,
                           Round,
                           Odds(Odds),
                           OddsAmount(OddsAmount),
                           MarketStatus(Active),
                           Selection(Selection),
                           getOdds)

maxLoss :: Rational
maxLoss = 10

marketBaseRate :: Rational
marketBaseRate = 0.065

data DumbState = DumbState (Maybe (Round, MarketId))
  deriving Eq

emptyDumbState :: DumbState
emptyDumbState = DumbState Nothing

dumb :: Monad m => Controller DumbState m
dumb game odds = do
  state <- get
  if shouldBet state
  then strategy
  else doNothing

  where newDumbState = DumbState $ Just (round game, marketId game)

        shouldBet state =
          (state /= newDumbState) &&
          (marketStatus game == Active)

        doNothing =
          return []

        isOddsAmountSuitable margin realOdds (OddsAmount _ amount) =
          expectedValue margin realOdds amount >= minimumIncrement

          where minimumIncrement = 0.01


        makeBet (Selection selectionId _ toBack _, realOdds) =
          if (margin > 0) && isOddsAmountSuitable margin realOdds oddsAmount
          then [BetActions [BetPlace selectionId Lay oddsAmount]]
          else []

          where (maxBackOdds@(Odds maxBackOddsValue), _) =
                  calculateCheapestOdds realOdds
                amount = Amount $ roundDown (maxLoss / (maxBackOddsValue - 1))
                oddsAmount = OddsAmount maxBackOdds amount
                bestAvailableBackOdds = fmap getOdds $ listToMaybe toBack
                (Just margin, _) =
                  calculateMargin realOdds bestAvailableBackOdds Nothing

        selectionList = toList $ selections game
        bets = take 1 $
               concatMap makeBet $
               zipSelectionsAndOdds selectionList odds
        strategy = do
          when (length bets > 0) $
            put newDumbState
          return bets

-- Round down to nearest cent
roundDown :: Rational -> Rational
roundDown value =
  fromIntegral cents / decimalFactor

  where decimalFactor = 100 :: Rational
        cents = floor (value * decimalFactor) :: Int

-- Round to nearest two decimal places.
betfairRound :: Rational -> Rational
betfairRound value =
  if value < 0
  then error "betfairRound: Expected non-negative value."
  else fromIntegral (thousandths + thousandthsToAdd) / decimalFactor

  where decimalFactor = 1000
        decimalBase = 10

        thousandths =
          floor (value * decimalFactor) :: Int

        finalThousandth =
          rem thousandths decimalBase

        thousandthsToAdd =
          if finalThousandth >= 5
          then decimalBase - finalThousandth
          else negate thousandths

expectedValue :: Int -> Odds -> Amount -> Rational
expectedValue margin (Odds realOdds) (Amount amount) =
  expectedProfit - expectedLoss

  where marketBaseRateDeduction =
          1 - marketBaseRate

        marginDecimal = fromIntegral margin / 100

        probability = recip realOdds

        roundedAmount = roundDown amount

        loss = betfairRound $ product
          [realOdds - (1 + marginDecimal),
           roundedAmount]

        expectedLoss = probability * loss

        profit = betfairRound $ product
          [marketBaseRateDeduction,
           roundedAmount]

        expectedProfit = (1 - probability) * profit