{-# LANGUAGE OverloadedStrings #-}

module Betfair.Model.Game.Parsing where

import           Prelude (String,
                          Rational,
                          error,
                          fst,
                          head,
                          read,
                          toEnum)

import           Control.Applicative ((<$>))
import           Control.Monad (return)
import           Data.Decimal ()
import           Data.Function ((.), ($))
import           Data.Functor (fmap)
import           Data.List ((++))

import           Data.Text (Text, unpack)
import qualified Data.Vector as Vector (fromList)

import           Numeric (readFloat)

import           Text.XML
import           Text.XML.Cursor
import qualified Data.ByteString.Lazy as L

import           Betfair.Model.Game
  ( Game
  , Selection(Selection)
  , Game(Game)
  , MarketId(MarketId)
  , Amount(Amount)
  , OddsAmount(OddsAmount)
  , Odds(Odds)
  , MarketStatus(Active, Settled, SuspendedGameRoundOver, SuspendedGameSettling)
  , SelectionStatus(InPlay, Winner)
  , fromPlayedCards
  )

selectionsBack :: Name
selectionsBack = "{urn:betfair:games:api:v1}bestAvailableToBackPrices"

selectionsLay :: Name
selectionsLay = "{urn:betfair:games:api:v1}bestAvailableToLayPrices"

market :: Name
market = "{urn:betfair:games:api:v1}market"

bettingWindowTime :: Name
bettingWindowTime = "{urn:betfair:games:api:v1}bettingWindowTime"

bettingWindowPercentageComplete :: Name
bettingWindowPercentageComplete = "{urn:betfair:games:api:v1}bettingWindowPercentageComplete"

parseGame :: L.ByteString -> Game
parseGame byteString =
  case parseGames of
    [singleGame] ->
      singleGame

    _ ->
      error "Expected to parse exactly one game."

  where document = parseLBS_ def byteString
        cursor = fromDocument document

        parseGames =
          do gameElement <- cursor $// element "{urn:betfair:games:api:v1}game"
             (bettingWindowTimeValue, bettingWindowPercentageCompleteValue) <- parseBettingWindow gameElement

             marketElement <- gameElement $// element market
             marketId <- MarketId <$> attribute "id" marketElement
             statusText <- marketElement $/ element "{urn:betfair:games:api:v1}status" >=> ($/ content)
             let cardList = parseCardList gameElement
             let constructor = Game marketId bettingWindowTimeValue bettingWindowPercentageCompleteValue

             return $ case parseMarketStatus statusText of
               Active ->
                 let selections = Vector.fromList $ selectionList marketElement
                 in constructor Active (fromPlayedCards cardList) selections

               otherStatus ->
                 constructor otherStatus (fromPlayedCards []) (Vector.fromList [])

        parseBettingWindow gameElement =
          do a <- readText (gameElement $/ element bettingWindowTime >=> ($/ content))
             b <- readText (gameElement $/ element bettingWindowPercentageComplete >=> ($/ content))
             return (a, b)

          where readText = fmap (read . unpack)

        parseCardList gameElement =
          do board <- gameElement $// element "{urn:betfair:games:api:v1}object"
             property <- board $/ element "{urn:betfair:games:api:v1}property"
             valueString <- attribute "value" property
             return . toEnum . read $ unpack valueString

        selectionList marketElement =
          do selectionsObject <- marketElement $/ element "{urn:betfair:games:api:v1}selections"
             selection <- selectionsObject $/ element "{urn:betfair:games:api:v1}selection"
             statusText <- selection $/ element "{urn:betfair:games:api:v1}status" >=> ($/ content)
             let selectionStatus = parseSelectionStatus statusText
             let backSelections = prices selectionsBack selection
             let laySelections = prices selectionsLay selection

             return $ Selection selectionStatus backSelections laySelections

        prices name selectionCursor =
          do oddsPrices <- selectionCursor $/ element name
             oddsPriceElement <- oddsPrices $/ element "{urn:betfair:games:api:v1}price"
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

  otherStatus ->
    error ("Unknown selection status: " ++ unpack otherStatus)

readRational :: String -> Rational
readRational = fst . head . readFloat
