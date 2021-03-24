{-# LANGUAGE OverloadedStrings #-}

module Betfair.IO (Login(Login),
                   ProxyChoice(UseProxy, NoProxy),
                   buildManagerSettings,
                   getAccount,
                   getGame,
                   postOrder) where

import Control.Lens ((^.), (&), (.~))
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy (unpack)
import Data.CaseInsensitive (CI)
import Data.Default.Class (def)
import Data.String (fromString)
import Data.Text (Text)
import Network.Connection (ProxySettings(SockSettingsSimple))
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options,
                     Response,
                     defaults,
                     getWith,
                     headers,
                     manager,
                     params,
                     postWith,
                     responseBody,
                     responseStatus,
                     statusCode)

data Login = Login { username :: String, password :: String }
  deriving Read

data ProxyChoice = UseProxy
                 | NoProxy
  deriving Read

accountUrl :: String
accountUrl = "https://api.games.betfair.com/rest/v1/account/snapshot"

gameUrl :: String
gameUrl = "https://api.games.betfair.com/rest/v1/channels/1444093/snapshot?selectionsType=CorrectPredictions"

orderUrl :: String
orderUrl = "https://api.games.betfair.com/rest/v1/bet/order"

parameterUsername :: Text
parameterUsername = "username"

headerApiPassword :: CI Strict.ByteString
headerApiPassword = "gamexAPIPassword"

headerApiAgent :: CI Strict.ByteString
headerApiAgent = "gamexAPIAgent"

headerApiAgentInstance :: CI Strict.ByteString
headerApiAgentInstance = "gamexAPIAgentInstance"

apiAgent :: Strict.ByteString
apiAgent = "justin.1.0"

apiAgentInstance :: Strict.ByteString
apiAgentInstance = "d8e8fca2dc0f896fd7cb4cb0031ba249"

proxySettings :: ProxySettings
proxySettings = SockSettingsSimple "localhost" 8000

buildManagerSettings :: ProxyChoice -> ManagerSettings
buildManagerSettings proxyChoice =
  mkManagerSettings def $ case proxyChoice of
    UseProxy ->
      Just proxySettings

    NoProxy ->
      Nothing

getAccount :: ManagerSettings -> Login -> IO Lazy.ByteString
getAccount managerSettings login = do
  response <- getWith options accountUrl
  return $ extractSuccessfulBody response

  where options = buildOptions managerSettings $ Just login

getGame :: ManagerSettings -> IO Lazy.ByteString
getGame managerSettings = do
  response <- getWith options gameUrl
  return $ extractSuccessfulBody response

  where options = buildOptions managerSettings Nothing

postOrder :: ManagerSettings -> Login -> Lazy.ByteString -> IO Lazy.ByteString
postOrder managerSettings login body = do
  response <- postWith options orderUrl body
  return $ extractSuccessfulBody response

  where options = buildOptions managerSettings $ Just login

extractSuccessfulBody :: Response Lazy.ByteString -> Lazy.ByteString
extractSuccessfulBody response =
  if responseCode == 200
  then body
  else error $
         "Unexpected status code: " ++ "\n" ++
         "Body:" ++ "\n" ++
         Lazy.unpack body

  where body = response ^. responseBody
        responseCode = response ^. responseStatus . statusCode

buildOptions :: ManagerSettings -> Maybe Login -> Options
buildOptions managerSettings maybeLogin =
  case maybeLogin of
    Nothing ->
      withManager

    Just login ->
      withManager &
        params .~ [(parameterUsername, fromString $ username login)] &
        headers .~ [(headerApiPassword, fromString $ password login),
                    (headerApiAgent, apiAgent),
                    (headerApiAgentInstance, apiAgentInstance)]

  where withManager = defaults & manager .~ Left managerSettings