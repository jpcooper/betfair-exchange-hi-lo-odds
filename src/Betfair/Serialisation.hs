{-# LANGUAGE OverloadedStrings #-}

module Betfair.Serialisation where

import Data.Ratio (denominator , numerator)
import Data.Text (Text)
import Numeric (readFloat)
import Text.XML

namespace :: Text
namespace = "urn:betfair:games:api:v1"

qualifyName :: Text -> Name
qualifyName name = Name name (Just namespace) Nothing

currency :: Text
currency = "EUR"

formatRational :: Rational -> Int -> String
formatRational rational decimalPlaces =
  if decimalPlaces == 0
  then show wholePart
  else shows wholePart ("." ++ take decimalPlaces (go remainder))

  where (wholePart, remainder) = num `quotRem` den
        num = numerator rational
        den = denominator rational

        go 0 = '0' : go 0
        go x = let (d, next) = (10 * x) `quotRem` den
               in shows d (go next)

readRational :: String -> Rational
readRational = fst . head . readFloat
