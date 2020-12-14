module Betfair.Controller.Dumb (Configuration(..),
                                DumbState,
                                dumb,
                                emptyDumbState) where

import Prelude (Eq,
                Int,
                Monad,
                Rational,
                (==),
                (/=),
                (>),
                (<=),
                (&&),
                (-),
                (*),
                (/),
                ($),
                concatMap,
                floor,
                fromIntegral,
                length,
                not,
                return,
                take)

import Control.Monad (when)
import Control.Monad.State.Lazy (get, put)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Vector (toList)

import Betfair.Controller.Common (getMinGreaterOdds,
                                  getMaxProfitableLayOdds,
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
                           Selection(Selection))

data Configuration = Configuration {maxLoss :: Amount}

data DumbState = DumbState (Maybe (Round, MarketId))
  deriving Eq

emptyDumbState :: DumbState
emptyDumbState = DumbState Nothing

dumb :: Monad m => Configuration -> Controller DumbState m
dumb configuration game odds = do
  state <- get
  if shouldBet state
  then strategy
  else doNothing

  where newDumbState = DumbState $ Just (round game, marketId game)

        shouldBet state =
          not haveAlreadyBetInRound && (marketStatus game == Active)

          where haveAlreadyBetInRound = state /= newDumbState

        doNothing =
          return []

        makeBet (Selection selectionId _ toBack _, realOdds) = case toBack of
          [] -> []
          (OddsAmount bestAvailableBackOdds _ : _) ->
            if potentialOdds <= maxProfitableLayOdds
            then [BetActions [BetPlace selectionId Lay oddsAmount]]
            else []

            where (Odds potentialOdds) =
                    getMinGreaterOdds bestAvailableBackOdds

                  Amount l = maxLoss configuration

                  Odds maxProfitableLayOdds =
                    getMaxProfitableLayOdds (Amount l) realOdds

                  amount =
                    Amount $ roundDown (l / (potentialOdds - 1))

                  oddsAmount =
                    OddsAmount (Odds potentialOdds) amount

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

-- Round to nearest hundredth.
-- If x.015, round up; otherwise down.
-- betfairRound :: Rational -> Rational
-- betfairRound value =
--   if value < 0
--   then error "betfairRound: Expected non-negative value."
--   else fromIntegral (thousandths + thousandthsToAdd) / decimalFactor

--   where decimalFactor = 1000
--         decimalBase = 10

--         thousandths =
--           floor (value * decimalFactor) :: Int

--         finalThousandth =
--           rem thousandths decimalBase

--         thousandthsToAdd =
--           if finalThousandth >= 5
--           then decimalBase - finalThousandth
--           else negate thousandths

-- expectedValue :: Int -> Odds -> Amount -> Rational
-- expectedValue margin (Odds realOdds) (Amount amount) =
--   expectedProfit - expectedLoss

--   where marketBaseRateDeduction =
--           1 - marketBaseRate

--         marginDecimal = fromIntegral margin / 100

--         probability = recip realOdds

--         roundedAmount = roundDown amount

--         loss = betfairRound $ product
--           [realOdds - (1 + marginDecimal),
--            roundedAmount]

--         expectedLoss = probability * loss

--         profit = betfairRound $ product
--           [marketBaseRateDeduction,
--            roundedAmount]

--         expectedProfit = (1 - probability) * profit