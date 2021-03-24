module Betfair.App.Common where

import           Betfair.Game (Amount(Amount))

maxLoss :: Amount
maxLoss = Amount 10

minBet :: Amount
minBet = Amount 2