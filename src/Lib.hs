{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

fred = User 3 "Fred" "Flintstone"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , fred
        ]

-- | Public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | private data that needs protection
newtype PrivateData = PrivateData { shh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | A type to wrap our public API.
type PublicAPI = Get '[JSON] [PublicData]

-- | A type to wrap our private API.
type PrivateAPI = Get '[JSON] PrivateData

type UsersAPI = "users" :> Get '[JSON] [User]

-- | our API
type BasicAPI
    = "public"  :> PublicAPI
 :<|> UsersAPI
 :<|> "private" :> BasicAuth "protected-realm" User :> PrivateAPI

type API = BasicAPI

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "fred" && password == "password"
        then return (Authorized fred)
        else return Unauthorized
  in BasicAuthCheck check

-- |
-- We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck'
-- value tagged with "foo-tag". This context is then supplied to 'server' and
-- threaded to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

api :: Proxy API
api = Proxy

-- |
-- An implementation of our server. Here is where we pass all the handlers to
-- our endpoints. In particular, for the BasicAuth protected handler, we need
-- to supply a function that takes 'User' as an argument.
basicAuthServer :: Server API
basicAuthServer =
  let publicAPIHandler = return [PublicData "foo", PublicData "bar"]
      usersHandler = return users
      privateAPIHandler (user :: User) = return (PrivateData {
        shh = Text.pack (show user)
      })
  in publicAPIHandler :<|> usersHandler :<|> privateAPIHandler

server :: Server API
server = basicAuthServer

app :: Application
app = serveWithContext api basicAuthServerContext server

port = 8080

-- | hello, server!
startApp :: IO ()
startApp = do
  putStrLn $ "Listening on " ++ show port
  run port app
