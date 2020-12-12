module Betfair.Controller.Dumb (DumbState, dumb, emptyDumbState) where

import Prelude (Eq,
                Monad,
                Rational,
                (==),
                (/=),
                (>),
                (>=),
                -- (<=),
                (&&),
                (-),
                (*),
                (/),
                ($),
                concatMap,
                fmap,
                fromIntegral,
                length,
                product,
                recip,
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

        isOddsAmountSuitable
          margin
          (Odds realOdds)
          (OddsAmount _ (Amount amount)) =

          -- isUnderMaxBet &&
          isPositiveExpectedValue

          where -- isUnderMaxBet =
                --   (theseOdds * amount) <= maxLoss

                isPositiveExpectedValue =
                  product [recip realOdds,
                           fromIntegral $ 100 * margin,
                           1 - marketBaseRate,
                           amount] >= 0.01

        makeBet (Selection selectionId _ toBack _, realOdds) =
          if (margin > 0) && isOddsAmountSuitable margin realOdds oddsAmount
          then [BetActions [BetPlace selectionId Lay oddsAmount]]
          else []

          where (maxBackOdds@(Odds maxBackOddsValue), _) =
                  calculateCheapestOdds realOdds
                -- oddsAmount = OddsAmount maxBackOdds (Amount 2)
                amount = Amount (maxLoss / (maxBackOddsValue - 1))
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
