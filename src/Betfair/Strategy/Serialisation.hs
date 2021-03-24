{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.Strategy.Serialisation where

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
import qualified Data.Text as Text (pack, unpack)
import           Text.XML
import           Text.XML.Cursor

import           Betfair.Game (Amount(..),
                               OddsAmount(..),
                               SelectionId(..),
                               MarketId(..),
                               Round(..))
import           Betfair.Serialisation (currency,
                                        formatRational,
                                        namespace,
                                        qualifyName,
                                        readRational)
import           Betfair.Game.Serialisation (decimalPlaces, formatOdds)
import           Betfair.Strategy (Account(Account),
                                   Bet(Bet),
                                   BetId(BetId),
                                   BetAction(..),
                                   BetActionResult(..),
                                   BetOrder(..),
                                   ResponseBetOrder(..),
                                   ResponseBetOrderErrorType(..),
                                   Side(Back, Lay))

-- Bool for whether to format pretty
formatBetOrder :: Bool -> BetOrder -> Lazy.ByteString
formatBetOrder isFormatPretty (BetOrder marketId thisRound betActions) =
  renderLBS (def {rsPretty = isFormatPretty}) document

  where document = Document prologue root epilogue
        prologue = Prologue [] Nothing []
        epilogue = []
        root =
          Element "postBetOrder"
            [("xmlns", namespace),
             ("marketId", getMarketId marketId),
             ("round", Text.pack $ show $ getRound thisRound),
             ("currency", currency)] $
            map (NodeElement . betActionToXMLElement) betActions

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
  (BetPlace (Bet
     (SelectionId thisSelectionId)
     thisSide
     (OddsAmount
        thisOdds
        (Amount thisAmount)))) =
  Element "betPlace" [] $ map NodeElement
    [bidTypeElement thisSide,
     priceElement,
     sizeElement,
     selectionIdElement]

  where priceElement =
          Element "price" [] $
            [NodeContent $ Text.pack $ formatOdds thisOdds decimalPlaces]

        sizeElement =
          Element "size" [] $
            [NodeContent $ Text.pack $ formatRational thisAmount decimalPlaces]

        selectionIdElement =
          Element "selectionId" [] $
            [NodeContent $ Text.pack thisSelectionId]

parseAccount :: Lazy.ByteString -> Account
parseAccount input =
  case result of
    [singleAccount] ->
      singleAccount

    otherResult ->
      error $ "Expected a single account: " ++ show otherResult

  where document = parseLBS_ def input
        cursor = fromDocument document

        result = do
          accountSnapshot <-
            element (qualifyName "accountSnapshot") cursor
          availableToBetBalanceElement <-
            accountSnapshot $/ element (qualifyName "availableToBetBalance")
          availableToBetBalanceContent <-
            availableToBetBalanceElement $/ content

          return $
            Account $ Amount $ readRational $ Text.unpack availableToBetBalanceContent

parseResponseBetOrder :: Lazy.ByteString -> ResponseBetOrder
parseResponseBetOrder input = do
  case (parseError, parseResponse) of
    ([singleError], []) ->
      singleError

    ([], [singleResponse]) ->
      singleResponse

    _ ->
      error $ "Expected either single error or single response: " ++ Char8.unpack input

  where document = parseLBS_ def input
        cursor = fromDocument document

        parseError = do
          thisError <- element "error" cursor
          errorCode <- thisError $/ element "errorCode"
          errorCodeContent <- errorCode $/ content

          return $ ResponseBetOrderError $ case errorCodeContent of
            "BETTING_ROUND_STALE" ->
              BettingRoundStale

            "INVALID_MARKET_ID" ->
              InvalidMarketId

            other ->
              BetOrderErrorOther other

        parseResponse = do
          response <- element (qualifyName "responseBetOrder") cursor

          return $ ResponseBetOrder $ results response

          where results response = do
                  betPlacementResult <-
                    response $/ element (qualifyName "betPlacementResult")
                  resultCodeElement <-
                    betPlacementResult $/ element (qualifyName "resultCode")
                  resultCodeContent <-
                    resultCodeElement $/ content

                  case resultCodeContent of
                    "OK" -> do
                      betId <-
                        betPlacementResult $/ element (qualifyName "betId")

                      betIdContent <- betId $/ content

                      return $ BetPlacementOkay $ BetId betIdContent

                    "EXPOSURE_OR_AVAILABLE_BALANCE_EXCEEDED" ->
                      return ExposureOrAvailableBalanceExceeded

                    other ->
                      return $ BetActionResultOther other
