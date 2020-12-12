{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Betfair.Model.Game.Parsing where

import           Prelude (Int,
                          String,
                          Rational,
                          Maybe(Just, Nothing),
                          Bool,
                          error,
                          fst,
                          head,
                          map,
                          read,
                          show,
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
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (pack)

import           Betfair.Controller.Controller (BetAction(BetPlace),
                                                Command(BetActions),
                                                Side(Back, Lay))
import           Betfair.Model.Game
  ( Selection(Selection)
  , Game(Game, marketId, round)
  , MarketId(MarketId)
  , Round(Round)
  , Amount(Amount)
  , OddsAmount(OddsAmount)
  , Odds(Odds)
  , MarketStatus(Active, Settled, SuspendedGameRoundOver, SuspendedGameSettling)
  , SelectionId(SelectionId)
  , SelectionStatus(InPlay, Winner)
  , getMarketId
  , getRound
  , formatRational
  , formatOdds
  , fromPlayedCards
  )

namespace :: Text
namespace = "urn:betfair:games:api:v1"

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

qualifyName :: Text -> Name
qualifyName name = Name name (Just namespace) Nothing

decimalPlaces :: Int
decimalPlaces = 2

currency :: Text
currency = "EUR"

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
             round <- (Round . read . unpack) <$> (roundElement $/ content)
             marketId <- MarketId <$> attribute "id" marketElement
             statusText <- marketElement $/ element (qualifyName "status") >=> ($/ content)
             let cardList = parseCardList gameElement
             let constructor = Game round marketId bettingWindowTimeValue bettingWindowPercentageCompleteValue

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

  otherStatus ->
    error ("Unknown selection status: " ++ unpack otherStatus)

readRational :: String -> Rational
readRational = fst . head . readFloat

bidTypeElement :: Side -> Element
bidTypeElement side =
  Element "bidType" [] [NodeContent sideText]

  where sideText = case side of
          Back ->
            "BACK"

          Lay ->
            "LAY"

betActionToXMLElement :: BetAction -> Element
betActionToXMLElement
  (BetPlace
     (SelectionId thisSelectionId)
     thisSide
     (OddsAmount
        thisOdds
        (Amount thisAmount))) =
  Element "betPlace" [] $ map NodeElement
                [bidTypeElement thisSide,
                 priceElement,
                 sizeElement,
                 selectionIdElement]

  where priceElement =
          Element "price" [] $
            [NodeContent $ pack $ formatOdds thisOdds decimalPlaces]

        sizeElement =
          Element "size" [] $
            [NodeContent $ pack $ formatRational thisAmount decimalPlaces]

        selectionIdElement =
          Element "selectionId" [] $
            [NodeContent $ pack thisSelectionId]

-- Bool for whether to format pretty
formatCommand :: Bool -> Game -> Command -> Lazy.ByteString
formatCommand isFormatPretty (Game {..}) command =
  case command of
     BetActions bets->
      renderLBS (def {rsPretty = isFormatPretty}) document

      where document = Document prologue root epilogue
            prologue = Prologue [] Nothing []
            epilogue = []
            root =
              Element "postBetOrder"
                [("xmlns", namespace),
                 ("marketId", getMarketId marketId),
                 ("round", pack $ show $ getRound round),
                 ("currency", currency)] $
                map (NodeElement . betActionToXMLElement) bets
