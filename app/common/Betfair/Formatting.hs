module Betfair.Formatting (formatGameAndOdds) where

import Prelude (Int,
                Integer,
                Maybe,
                RealFrac,
                Rational,
                String,
                (<=),
                (.),
                (+),
                (-),
                (*),
                (/),
                ceiling,
                error,
                filter,
                floor,
                fmap,
                fromIntegral,
                fst,
                head,
                maybe,
                show,
                snd)

import Data.Default.Class (def)
import Data.Eq ((==))
import Data.Function (($))
import Data.List ((++),
                  length,
                  replicate,
                  reverse,
                  map,
                  zip,
                  dropWhile,
                  repeat,
                  unlines)
import Data.Maybe (listToMaybe)
import Data.Vector (toList)
import Text.Layout.Table (column, expand, left, gridString)
import Text.Layout.Table.Spec.AlignSpec (charAlign)

import Betfair.Model.Game
  ( Game(Game)
  , Odds(Odds)
  , OddsAmount(OddsAmount)
  , Selection(Selection)
  , SelectionStatus(InPlay, Winner)
  , board
  , formatOdds
  , selections
  , selectionStatus
  , marketStatus
  )

decimalPlacesRealOdds :: Int
decimalPlacesRealOdds = 3

header :: [String]
header = ["BACK", "", "", "MARGIN", "ODDS", "MARGIN", "LAY", "", ""]

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

-- The minimum increment possible in betting. Every larger increment
-- is a multiple of this minimum increment.
minimumIncrement :: Rational
minimumIncrement = 0.01

formatGameAndOdds :: Game -> [Odds] -> String
formatGameAndOdds Game {..} odds =
  unlines [show marketStatus, "\n", gridString (replicate (length header) colSpec) (header : rows)]

  where colSpec = column expand left (charAlign '.') def
        selectionsAndOdds = zipSelectionsAndOdds (toList selections) odds
        rows = map (\(s, p) -> formatSelectionAndOdds s p) selectionsAndOdds

zipSelectionsAndOdds :: [Selection] -> [Odds] -> [(Selection, Odds)]
zipSelectionsAndOdds selections odds =
  zip (selectionsInPlay ++ emptySelections) odds

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

        emptySelections = repeat (Selection InPlay [] [])

formatSelectionAndOdds :: Selection -> Odds -> [String]
formatSelectionAndOdds selection odds =
  showSelections (reverse toBack) ++
  [ formatMargin backMarginResult
  , formatOdds odds decimalPlacesRealOdds
  , formatMargin layMarginResult
  ] ++
  showSelections toLay

  where Selection _ toBack toLay = selection
        showSelections = map show
        getOdds (OddsAmount theseOdds _) = theseOdds
        backOdds = fmap getOdds $ listToMaybe toBack
        layOdds = fmap getOdds $ listToMaybe toLay
        (backMarginResult, layMarginResult) =
          calculateMargin odds backOdds layOdds
        formatMargin = maybe "X" show

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
  (Odds maxBackOdds, Odds maxLayOdds)

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
        maxLayOdds =
          fromIntegral (ceiling (approximateMinLayOdds / layIncrement) :: Integer) * layIncrement

        getIncrement odds =
          snd $ head $ filter ((odds <=) . fst) oddsIncrements
