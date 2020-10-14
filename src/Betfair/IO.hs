{-# LANGUAGE OverloadedStrings #-}

module Betfair.IO (getGame) where

import Prelude (IO, String, error)

import Control.Lens ((^.))
import Control.Monad (return)
import Data.ByteString.Lazy (ByteString)
import Data.Eq ((==))
import Data.Function ((.))
import Network.Wreq (get, responseBody, responseStatus, statusCode)

getGame :: String -> IO ByteString
getGame gameUrl =
  do response <- get gameUrl
     let body = response ^. responseBody
     let responseCode = response ^. responseStatus . statusCode

     if (responseCode == 200)
       then return body
       else error "Status code not 200."
