{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Client
import Server (app)

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS8
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

users :: ResponseMatcher
users = [json|
  [
    {
      "userId": 1,
      "userFirstName":"Isaac",
      "userLastName":"Newton"
    },
    {
      "userId": 2,
      "userFirstName": "Albert",
      "userLastName": "Einstein"
    },
    {
      "userId": 3,
      "userFirstName": "Fred",
      "userLastName": "Flintstone"
    }
  ]
|]

spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            get "/users" `shouldRespondWith` users

spec2 :: Spec
spec2 =
  describe "POST /tickets.json" $ do
    it "responds okay with a subject" $ do
      subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
      username <- Env.getEnv "ZENDESK_USERNAME"
      password <- Env.getEnv "ZENDESK_PASSWORD"
      let ticketCommentCreate = TicketCommentCreate (Just "A body") Nothing Nothing Nothing
      let ticketCreate = TicketCreate (Just "A subject") ticketCommentCreate
      run subdomain (BS8.pack username) (BS8.pack password) (\auth -> createTicket auth ticketCreate)
        `shouldReturn` Right (TicketCreateResponse Nothing)
      -- XXX: Underlying HTTP response code should be 401/Created.
    it "responds okay without a subject" $ do
      subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
      username <- Env.getEnv "ZENDESK_USERNAME"
      password <- Env.getEnv "ZENDESK_PASSWORD"
      let ticketCommentCreate = TicketCommentCreate (Just "A body") Nothing Nothing Nothing
      let ticketCreate = TicketCreate Nothing ticketCommentCreate
      run subdomain (BS8.pack username) (BS8.pack password) (\auth -> createTicket auth ticketCreate)
        `shouldReturn` Right (TicketCreateResponse Nothing)
      -- XXX: Underlying HTTP response code should be 401/Created.

main :: IO ()
main = hspec $ do
  spec
  spec2
