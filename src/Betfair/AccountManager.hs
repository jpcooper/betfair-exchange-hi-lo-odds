{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Betfair.AccountManager where

import Data.Proxy (Proxy)

import Betfair.Game (Game)
import Betfair.Strategy (Account, BetOrder, ResponseBetOrder)

class Monad m => AccountManager am m where
  type Result am (m :: * -> *) a

  getAccount :: Proxy m -> am -> Result am m Account

  processGame :: Proxy m -> am -> Game -> Result am m ()

  processBetOrder :: Proxy m -> am -> Game -> BetOrder -> Result am m ResponseBetOrder