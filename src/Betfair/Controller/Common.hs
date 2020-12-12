module Betfair.Controller.Common (calculateMargin,
                                  calculateCheapestOdds,
                                  zipSelectionsAndOdds) where

import Prelude (Int,
                Integer,
                Maybe,
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
                fmap,
                fromIntegral,
                fst,
                head,
                map,
                snd,
                zip)

import Betfair.Model.Game (Odds(Odds),
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

-- What are the margins of the available odds to relative to the
-- minimum and maximum possible odds on either side of the real odds?
calculateMargin :: Odds -> Maybe Odds -> Maybe Odds -> (Maybe Int, Maybe Int)
calculateMargin realOdds maxAvailableBack minAvailableLay =
  (backMargin, layMargin)

  where (Odds maxPossibleBack, Odds minPossibleLay) = calculateCheapestOdds realOdds
        getOdds (Odds theseOdds) = theseOdds
        backMargin = fmap (toPips . (maxPossibleBack -) . getOdds) maxAvailableBack
        subtractLay :: Rational -> Rational
        subtractLay x = (x - minPossibleLay)
        layMargin = fmap (toPips . subtractLay . getOdds) minAvailableLay

-- Calculate the cheapest odds for back and lay which would make a
-- profit given the real odds of an outcome, basing this on the
-- increments as defined in `oddsIncrements`.
calculateCheapestOdds :: Odds -> (Odds, Odds)
calculateCheapestOdds (Odds realOdds) =
  (Odds maxBackOdds, Odds minLayOdds)

  where -- Maximum odds less than realOdds which are a whole multiple
        -- of minimumIncrement
        approximateMaxBackOdds =
          fromIntegral (ceiling ((realOdds - minimumIncrement) / minimumIncrement) :: Integer) * minimumIncrement

        -- Minimum odds greater than realOdds which are a whole
        -- multiple of minimumIncrement
        approximateMinLayOdds =
          fromIntegral (floor ((realOdds + minimumIncrement) / minimumIncrement) :: Integer) * minimumIncrement

        backIncrement = getIncrement approximateMaxBackOdds
        layIncrement = getIncrement approximateMinLayOdds

        -- Maximum possible back odds which would be profitable
        maxBackOdds =
          fromIntegral (floor (approximateMaxBackOdds / backIncrement) :: Integer) * backIncrement

        -- Minimum possible lay odds which would be profitable
        minLayOdds =
          fromIntegral (ceiling (approximateMinLayOdds / layIncrement) :: Integer) * layIncrement

        getIncrement odds =
         snd $ head $ filter ((odds <=) . fst) oddsIncrements

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