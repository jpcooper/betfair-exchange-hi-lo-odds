{-# LANGUAGE RecordWildCards #-}

module Betfair.App.Common.Formatting (formatBetOrders, formatGameAndOdds) where

import Prelude (Bool(True),
                Int,
                String,
                (.),
                (<$>),
                maybe,
                show)

import qualified Data.ByteString.Lazy.Char8 as Lazy (unpack)
import Data.Default.Class (def)
import Data.Function (($))
import Data.List ((++),
                  length,
                  replicate,
                  reverse,
                  map,
                  unlines)
import Data.Maybe (listToMaybe)
import Text.Layout.Table (column, expand, left, gridString)
import Text.Layout.Table.Spec.AlignSpec (charAlign)

import Betfair.Strategy (BetOrder, calculateMargin, zipSelectionsAndOdds)
import Betfair.Game
  ( Amount
  , Game(Game)
  , Odds
  , OddsAmount(OddsAmount)
  , Selection(Selection)
  , board
  , selections
  , marketStatus
  )
import Betfair.Game.Serialisation (formatOdds)
import Betfair.Strategy.Serialisation (formatBetOrder)

decimalPlacesRealOdds :: Int
decimalPlacesRealOdds = 3

header :: [String]
header = ["BACK", "", "", "MARGIN", "ODDS", "LAY", "", ""]

formatGameAndOdds :: Amount -> Game -> [Odds] -> String
formatGameAndOdds maxLoss Game {..} odds =
  unlines [show marketStatus, "\n", gridString (replicate (length header) colSpec) (header : rows)]

  where colSpec = column expand left (charAlign '.') def
        selectionsAndOdds = zipSelectionsAndOdds selections odds
        rows = map (\(s, o) -> formatSelectionAndOdds maxLoss s o) selectionsAndOdds

formatSelectionAndOdds :: Amount -> Selection -> Odds -> [String]
formatSelectionAndOdds maxLoss selection odds =
  showSelections (reverse toBack) ++
  [ formatMargin backMarginResult
  , formatOdds odds decimalPlacesRealOdds
  ] ++
  showSelections toLay

  where Selection _ _ toBack toLay = selection
        showSelections = map show
        getOdds (OddsAmount theseOdds _) = theseOdds
        backOdds = getOdds <$> listToMaybe toBack
        backMarginResult =
          calculateMargin maxLoss odds <$> backOdds
        formatMargin = maybe "X" show

formatBetOrders :: [BetOrder] -> String
formatBetOrders commands =
  unlines $ map (Lazy.unpack . formatBetOrder isFormatPretty) commands

  where isFormatPretty = True