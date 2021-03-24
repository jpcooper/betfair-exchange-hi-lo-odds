{-# LANGUAGE RecordWildCards #-}

module Betfair.Strategy where

import           Control.Monad.State.Lazy (StateT)
import           Data.Text (Text)

import           Betfair.Game (Amount(Amount),
                               Game,
                               Odds(Odds),
                               OddsAmount(OddsAmount),
                               MarketId,
                               MarketStatus,
                               Round,
                               Selection(..),
                               SelectionId(..),
                               SelectionStatus(..))
import qualified Betfair.Game as Game (Game(marketId,
                                            marketStatus,
                                            round,
                                            selections))

type Strategy s m = Account -> GameRepresentation -> [Odds] -> StateT s m [BetOrder]

data Account = Account {
  balance :: Amount}
  deriving Show

data GameRepresentation = GameRepresentation {
  marketId :: MarketId,
  marketStatus :: MarketStatus,
  round :: Round,
  inPlaySelections :: [SelectionId]}

data Side = Back | Lay
  deriving Show

data Bet = Bet SelectionId Side OddsAmount
  deriving Show

data BetAction = BetPlace Bet
  deriving Show

data BetOrder = BetOrder MarketId Round [BetAction]
  deriving Show

data BetId = BetId Text
  deriving Show

data BetActionResult = BetPlacementOkay BetId
                     | ExposureOrAvailableBalanceExceeded
                     | BetActionResultOther Text
  deriving Show

data ResponseBetOrderErrorType = BettingRoundStale
                               | InvalidMarketId
                               | BetOrderErrorOther Text
  deriving Show

data ResponseBetOrder = ResponseBetOrder [BetActionResult]
                      | ResponseBetOrderError ResponseBetOrderErrorType
  deriving Show

-- The minimum increment possible in betting. Every larger increment
-- is a multiple of this minimum increment.
minimumIncrement :: Rational
minimumIncrement = 0.01

-- Bets must be made odds with an increment based on the range within
-- which they fall, as defined below. For instance, bets on odds up to
-- 2 are made with an increment of 0.01. Bets on odds above 2 and up
-- to 3 are made with an increment of 0.02.
oddsIncrements :: [(Rational, Rational)]
oddsIncrements =
  [(2   ,  0.01),
   (3   ,  0.02),
   (5   ,  0.05),
   (10  ,  0.10),
   (20  ,  0.20),
   (30  ,  0.50),
   (50  ,  1.00),
   (100 ,  2.00),
   (500 ,  5.00),
   (1000, 10.00)]

buildGameRepresentation :: Game -> GameRepresentation
buildGameRepresentation game =
  GameRepresentation
    (Game.marketId game)
    (Game.marketStatus game)
    (Game.round game) $
    map selectionId $
      getInPlaySelections $
        Game.selections game

toPips :: RealFrac a => a -> Int
toPips a = floor (a * 100)

marketBaseRate :: Rational
marketBaseRate = 0.03

-- What are the margins of the available back odds to relative to the
-- maximum profitable lay odds?
calculateMargin :: Amount -> Odds -> Odds -> Int
calculateMargin maxLoss realOdds (Odds maxAvailableBack) =
  toPips $ (maxProfitableLayOdds - maxAvailableBack)

  where Odds maxProfitableLayOdds = getMaxProfitableLayOdds maxLoss realOdds

getIncrement :: Odds -> Rational
getIncrement (Odds odds) =
  case filter ((odds <=) . fst) oddsIncrements of
    [] ->
      error "getIncrement: odds greater than max."

    filtered ->
      snd $ head filtered

getMaxProfitableLayOdds :: Amount -> Odds -> Odds
getMaxProfitableLayOdds (Amount maxLoss) (Odds realOdds) =
  Odds (fromIntegral floored * incrementForRange)

  where l = maxLoss
        r = -0.05
        s =  0.05
        z = realOdds
        betAmount = (l - s) / (z - 1)
        a = betAmount
        d = 1 - marketBaseRate
        p = recip realOdds

        x = a * (d - (d * p) + p)
        y = p * (r + s)

        approximateResult = (x - y + r - minimumIncrement) / (a * p)
        incrementForRange = getIncrement (Odds approximateResult)
        floored = floor (approximateResult / incrementForRange) :: Int

getMaxSmallerOdds :: Odds -> Odds
getMaxSmallerOdds (Odds realOdds) =
  Odds (fromIntegral b * incrementForRange)

  where -- Maximum odds less than odds which are a whole multiple
        -- of minimumIncrement
        a = ceiling ((realOdds - minimumIncrement) / minimumIncrement) :: Int
        approximateResult =
          fromIntegral a * minimumIncrement

        b = floor (approximateResult / incrementForRange) :: Int
        incrementForRange = getIncrement (Odds approximateResult)

getMinGreaterOdds :: Odds -> Odds
getMinGreaterOdds (Odds realOdds) =
  Odds (fromIntegral b * incrementForRange)

  where -- Minimum odds greater than realOdds which are a whole
        -- multiple of minimumIncrement
        a = floor ((realOdds + minimumIncrement) / minimumIncrement) :: Int
        approximateResult =
          fromIntegral a * minimumIncrement

        -- Minimum possible lay odds which would be profitable
        b = ceiling (approximateResult / incrementForRange) :: Int
        incrementForRange = getIncrement (Odds approximateResult)

zipSelectionsAndOdds :: [Selection] -> [Odds] -> [(Selection, Odds)]
zipSelectionsAndOdds selections odds =
  zip (getInPlaySelections selections) odds

getInPlaySelections :: [Selection] -> [Selection]
getInPlaySelections selections =
  if all isSelectionInPlay result
  then result
  else error "Expected all associations after winners and losers to be in play."

  where result =
          dropWhile isSelectionWinnerOrLoser selections

        isSelectionWinnerOrLoser Selection {..} =
          (selectionStatus == Winner) || (selectionStatus == Loser)

        isSelectionInPlay Selection {..} =
          selectionStatus == InPlay

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

calculateBackLoss :: OddsAmount -> Amount
calculateBackLoss (OddsAmount _ amount) =
  amount

calculateLayLoss :: OddsAmount -> Amount
calculateLayLoss (OddsAmount (Odds odds) (Amount amount)) =
  Amount $ (odds - 1) * amount

calculateLoss :: Side -> OddsAmount -> Amount
calculateLoss side oddsAmount =
  case side of
    Back ->
      calculateBackLoss oddsAmount

    Lay ->
      calculateLayLoss oddsAmount