module Betfair.Controller.Controller where

import Prelude (Show)

import Control.Monad.State.Lazy (StateT)

import Betfair.Model.Game (Game,
                           Odds,
                           OddsAmount,
                           SelectionId)

type Controller s m = Game -> [Odds] -> StateT s m [Command]

data Side = Back | Lay
  deriving Show

data BetAction = BetPlace SelectionId Side OddsAmount
  deriving Show

data Command = BetActions [BetAction]
  deriving Show
