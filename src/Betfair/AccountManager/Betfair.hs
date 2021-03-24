{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Betfair.AccountManager.Betfair where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HTTP.Client (ManagerSettings)

import           Betfair.AccountManager (AccountManager(..))
import           Betfair.IO (Login)
import qualified Betfair.IO as IO (getAccount, postOrder)
import           Betfair.Strategy.Serialisation (formatBetOrder,
                                                 parseAccount,
                                                 parseResponseBetOrder)

data Betfair = Betfair ManagerSettings Login

instance MonadIO m => AccountManager Betfair m where
  type Result Betfair m a = m a

  getAccount _ (Betfair managerSettings login) =
    parseAccount <$> liftIO (IO.getAccount managerSettings login)

  processGame _ _ _ =
    return ()

  processBetOrder _ (Betfair managerSettings login) _ betOrder =
    liftIO $
      parseResponseBetOrder <$>
        (IO.postOrder managerSettings login $
           formatBetOrder isFormatPretty betOrder)

    where isFormatPretty = False
