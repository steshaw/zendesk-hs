{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lib

import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp
import Servant

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
  Warp.run port app
