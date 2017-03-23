module Client where

import Lib (api, PublicData, User, PrivateData, TicketPage)

import qualified Data.ByteString as BS
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TLS
import Servant
import Servant.Client

getPublic :: ClientM [PublicData]
getUsers :: ClientM [User]
getPrivate :: BasicAuthData -> ClientM PrivateData
getTickets :: BasicAuthData -> ClientM TicketPage

(getPublic :<|>
 getUsers :<|>
 getPrivate :<|>
 getTickets) = client api

runner :: BaseUrl -> BS.ByteString -> BS.ByteString -> (BasicAuthData -> ClientM a) -> IO (Either ServantError a)
runner baseUrl username password authenticatedAction = do
  manager <- HttpClient.newManager TLS.tlsManagerSettings
  let basicAuthData = BasicAuthData username password
  runClientM (authenticatedAction basicAuthData) (ClientEnv manager baseUrl)

localBaseUrl = BaseUrl Http "localhost" 8080 ""
zendeskBaseUrl subdomain = BaseUrl Https (subdomain ++ ".zendesk.com") 443 "/api/v2"

runLocal = runner localBaseUrl
run subdomain = runner (zendeskBaseUrl subdomain)
