module Zendesk.Client where

import Zendesk.API

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TLS
import Servant
import Servant.Client
import qualified System.Environment as Env

getUsers :: BasicAuthData -> ClientM Users
getTickets :: BasicAuthData -> ClientM TicketPage
createTicket :: BasicAuthData -> TicketCreate -> ClientM TicketCreateResponse

getUsers
  :<|> getTickets
  :<|> createTicket = client api

runner :: BaseUrl -> BS.ByteString -> BS.ByteString -> (BasicAuthData -> ClientM a) -> IO (Either ClientError a)
runner baseUrl username password authenticatedAction = do
  manager <- HttpClient.newManager TLS.tlsManagerSettings
  let basicAuthData = BasicAuthData username password
  runClientM (authenticatedAction basicAuthData) (mkClientEnv manager baseUrl)

mockServerBaseUrl :: BaseUrl
mockServerBaseUrl = BaseUrl Http "localhost" 8080 ""

zendeskBaseUrl ::
  String       -- The Zendesk subdomain.
  -> BaseUrl   -- The `BaseUrl` to use with `run`.
zendeskBaseUrl subdomain = BaseUrl Https (subdomain <> ".zendesk.com") 443 "/api/v2"

runMock
  :: BSC8.ByteString
  -> BSC8.ByteString
  -> (BasicAuthData -> ClientM a)
  -> IO (Either ClientError a)
runMock = runner mockServerBaseUrl

run :: [Char]
       -> BSC8.ByteString
       -> BSC8.ByteString
       -> (BasicAuthData -> ClientM a)
       -> IO (Either ClientError a)
run subdomain = runner (zendeskBaseUrl subdomain)

-- |
--
-- Extracts the subdomain, username and password from the environment via
-- 'Env.getEnv'.
--
-- Requires the environment variables to be set:
--
-- * ZENDESK_SUBDOMAIN
-- * ZENDESK_USERNAME
-- * ZENDESK_PASSWORD
--
env :: IO (String, BSC8.ByteString, BSC8.ByteString)
env = do
  subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
  username <- Env.getEnv "ZENDESK_USERNAME"
  password <- Env.getEnv "ZENDESK_PASSWORD"
  return (subdomain, BSC8.pack username, BSC8.pack password)
