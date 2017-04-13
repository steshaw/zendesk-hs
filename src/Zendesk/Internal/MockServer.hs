{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Zendesk.Internal.MockServer where

import Zendesk

import Data.Monoid ((<>))
import qualified Network.Wai.Handler.Warp as Warp
import Servant

emptyUser = User Nothing Nothing Nothing Nothing

issac = emptyUser
  { userId = Just 1
  , userName = Just "Issac Newton"
  }

albert = emptyUser
  { userId = Just 2
  , userName = Just "Albert Einstein"
  }

fred = emptyUser
  { userId = Just 3
  , userName = Just "Fred Flintstone"
  , userEmail = Just "fred.flintstone@gmail.com"
  }

users :: [User]
users = [ issac, albert, fred ]

ticket1 = Ticket
  { ticketId = Just 1
  , ticketUrl = Nothing
  , ticketSubject = Just "This is a subject"
  , ticketDescription = Just "This is a description"
  }

ticket2 = Ticket
  { ticketId = Just 2
  , ticketUrl = Nothing
  , ticketSubject = Just "This is another subject"
  , ticketDescription = Just "This is another description"
  }

exampleTickets = [ticket1, ticket2]

ticketPage = TicketPage
  { ticketCount = length exampleTickets
  , ticketNextPage = Nothing
  , ticketPrevPage = Nothing
  , tickets = exampleTickets
  }

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

-- |
-- An implementation of our server. Here is where we pass all the handlers to
-- our endpoints. In particular, for the BasicAuth protected handler, we need
-- to supply a function that takes 'User' as an argument.
basicAuthServer :: Server API
basicAuthServer =
  let
      getUsers user = return (Users users)
      getTickets user = return ticketPage
      postTicket user ticketCreate = return (TicketCreateResponse (Just 0))
  in     getUsers
    :<|> getTickets
    :<|> postTicket

server :: Server API
server = basicAuthServer

app :: Application
app = serveWithContext api basicAuthServerContext server

port = 8080

-- | Start the mock server.
main :: IO ()
main = do
  putStrLn $ "Listening on " <> show port
  Warp.run port app
