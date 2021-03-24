{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Betfair.Game.Serialisation where

import           Data.Decimal ()
import           Data.Text (Text, unpack)
import           Text.XML
import           Text.XML.Cursor
import qualified Data.ByteString.Lazy as Lazy

import           Betfair.Serialisation (formatRational, qualifyName, readRational)
import           Betfair.Game
  ( Selection(Selection)
  , Game(Game)
  , MarketId(MarketId)
  , Round(Round)
  , Amount(Amount)
  , OddsAmount(OddsAmount)
  , Odds(Odds)
  , MarketStatus(Active, Settled, SuspendedGameRoundOver, SuspendedGameSettling)
  , SelectionId(SelectionId)
  , SelectionStatus(InPlay, Winner, Loser)
  , fromPlayedCards
  )

selectionsBack :: Name
selectionsBack = qualifyName "bestAvailableToBackPrices"

selectionsLay :: Name
selectionsLay = qualifyName "bestAvailableToLayPrices"

market :: Name
market = qualifyName "market"

bettingWindowTime :: Name
bettingWindowTime = qualifyName "bettingWindowTime"

bettingWindowPercentageComplete :: Name
bettingWindowPercentageComplete = qualifyName "bettingWindowPercentageComplete"

decimalPlaces :: Int
decimalPlaces = 2

parseGame :: Lazy.ByteString -> Game
parseGame byteString =
  case parseGames of
    [singleGame] ->
      singleGame

    _ ->
      error "Expected to parse exactly one game."

  where document = parseLBS_ def byteString
        cursor = fromDocument document

        parseGames =
          do gameElement <- cursor $// element (qualifyName "game")
             (bettingWindowTimeValue, bettingWindowPercentageCompleteValue) <- parseBettingWindow gameElement

             marketElement <- gameElement $// element market
             roundElement <- gameElement $// element (qualifyName "round")
             thisRound <- (Round . read . unpack) <$> (roundElement $/ content)
             marketId <- MarketId <$> attribute "id" marketElement
             statusText <- marketElement $/ element (qualifyName "status") >=> ($/ content)
             let cardList = parseCardList gameElement
             let constructor = Game thisRound marketId bettingWindowTimeValue bettingWindowPercentageCompleteValue
             let marketStatus = parseMarketStatus statusText
             let selections = selectionList marketElement

             return $ constructor marketStatus (fromPlayedCards cardList) selections

        parseBettingWindow gameElement =
          do a <- readText (gameElement $/ element bettingWindowTime >=> ($/ content))
             b <- readText (gameElement $/ element bettingWindowPercentageComplete >=> ($/ content))
             return (a, b)

          where readText = fmap (read . unpack)

        parseCardList gameElement =
          do board <- gameElement $// element (qualifyName "object")
             property <- board $/ element (qualifyName "property")
             valueString <- attribute "value" property
             return . toEnum . read $ unpack valueString

        selectionList marketElement =
          do selectionsObject <- marketElement $/ element (qualifyName "selections")
             selection <- selectionsObject $/ element (qualifyName "selection")
             statusText <- selection $/ element (qualifyName "status") >=> ($/ content)
             selectionId <- attribute "id" selection
             let selectionIdString = unpack selectionId
             let selectionStatus = parseSelectionStatus statusText
             let backSelections = prices selectionsBack selection
             let laySelections = prices selectionsLay selection

             return $ Selection
               (SelectionId selectionIdString)
               selectionStatus
               backSelections
               laySelections

        prices name selectionCursor =
          do oddsPrices <- selectionCursor $/ element name
             oddsPriceElement <- oddsPrices $/ element (qualifyName "price")
             oddsString <- oddsPriceElement $/ content
             priceString <- attribute "amountUnmatched" oddsPriceElement
             return $ OddsAmount (Odds $ readRational $ unpack oddsString) (Amount $ readRational $ unpack priceString)

parseMarketStatus :: Text -> MarketStatus
parseMarketStatus statusText = case statusText of
  "ACTIVE" ->
    Active

  "SUSPENDED_GAME_ROUND_OVER" ->
    SuspendedGameRoundOver

  "SUSPENDED_GAME_SETTLING" ->
    SuspendedGameSettling

  "SETTLED" ->
    Settled

  otherStatus ->
    error ("Unknown market status: " ++ unpack otherStatus)

parseSelectionStatus :: Text -> SelectionStatus
parseSelectionStatus statusText = case statusText of
  "IN_PLAY" ->
    InPlay

  "WINNER" ->
    Winner

  "LOSER" ->
    Loser

  otherStatus ->
    error ("Unknown selection status: " ++ unpack otherStatus)

formatOdds :: Odds -> Int -> String
formatOdds (Odds odds) theseDecimalPlaces =
  formatRational odds theseDecimalPlaces