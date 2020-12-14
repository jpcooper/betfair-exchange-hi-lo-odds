module Betfair.App.Common.Wrapper (getOdds) where

import Prelude (IO, Int, (==))

import Control.Monad (return)
import Data.Function (($))

import Betfair.Model.Game
  ( Game(Game)
  , Odds(Odds)
  , marketStatus
  , getNumberLower
  , getNumberRemaining
  , board
  , selections
  )
import Betfair.Prob.Interface ( createProbabilitiesResult
                              , freeProbabilitiesResult
                              , calculateOdds)

getOdds :: Game -> IO [Odds]
getOdds Game {..} =
  do let numberRemaining = getNumberRemaining board
     let numberLower = getNumberLower board
     numeratorsResult <- createProbabilitiesResult maxSize
     denominatorsResult <- createProbabilitiesResult maxSize
     oddsList <- calculateOdds
                   numeratorsResult
                   denominatorsResult
                   numberRemaining
                   numberLower
     freeProbabilitiesResult numeratorsResult
     freeProbabilitiesResult denominatorsResult

     return $ dropFirstOddsIfCertain oddsList

-- If the first odds are certain, then there will be no corresponding
-- selection to bet on, as this selection will have won and will be
-- disabled.
dropFirstOddsIfCertain :: [Odds] -> [Odds]
dropFirstOddsIfCertain oddsList =
  case oddsList of
    [] ->
      []

    (Odds firstOdds : rest) ->
      if firstOdds == 1
      then rest
      else oddsList

maxSize :: Int
maxSize = 13