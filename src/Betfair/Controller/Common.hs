module Betfair.Controller.Common (calculateMargin,
                                  getMaxProfitableLayOdds,
                                  getMaxSmallerOdds,
                                  getMinGreaterOdds,
                                  zipSelectionsAndOdds) where

import Prelude (Int,
                RealFrac,
                Rational,
                ($),
                (<=),
                (.),
                (+),
                (-),
                (*),
                (/),
                (==),
                ceiling,
                dropWhile,
                error,
                filter,
                floor,
                fromIntegral,
                fst,
                head,
                map,
                max,
                recip,
                snd,
                zip)

import Betfair.Model.Game (Amount(Amount),
                           Odds(Odds),
                           Selection(..),
                           SelectionStatus(..))

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

toPips :: RealFrac a => a -> Int
toPips a = floor (a * 100)

marketBaseRate :: Rational
marketBaseRate = 0.065

-- What are the margins of the available back odds to relative to the
-- maximum profitable lay odds?
calculateMargin :: Amount -> Odds -> Odds -> Int
calculateMargin maxLoss realOdds (Odds maxAvailableBack) =
  toPips $ max 0 (maxProfitableLayOdds - maxAvailableBack)

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
  zip selectionsInPlay odds

  where selectionsInPlay =
          map assertInPlay $ dropWhile isSelectionWinner selections

        isSelectionWinner Selection {..} =
          selectionStatus == Winner

        isSelectionInPlay Selection {..} =
          selectionStatus == InPlay

        assertInPlay selection =
          if (isSelectionInPlay selection)
          then selection
          else error "Expected all associations after winners to be in play."

        -- emptySelections = repeat (Selection (SelectionId "") InPlay [] [])