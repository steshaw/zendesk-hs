{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

usersSpec :: Spec
usersSpec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            get "/users" `shouldRespondWith` users

emptyCommentCreate = TicketCommentCreate
  { ticketCommentCreate_body = Nothing
  , ticketCommentCreate_htmlBody = Nothing
  , ticketCommentCreate_public = Nothing
  , ticketCommentCreate_authorId = Nothing
  }

ticketsSpec :: Spec
ticketsSpec =
  describe "POST /tickets.json" $ do

    it "responds okay with a subject" $ do
      subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
      username <- Env.getEnv "ZENDESK_USERNAME"
      password <- Env.getEnv "ZENDESK_PASSWORD"
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate (Just "A subject") comment
      run subdomain (BS8.pack username) (BS8.pack password) (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "responds okay without a subject" $ do
      subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
      username <- Env.getEnv "ZENDESK_USERNAME"
      password <- Env.getEnv "ZENDESK_PASSWORD"
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate Nothing comment
      run subdomain (BS8.pack username) (BS8.pack password) (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "can create a private ticket (with private comment)" $ do
      subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
      username <- Env.getEnv "ZENDESK_USERNAME"
      password <- Env.getEnv "ZENDESK_PASSWORD"
      let comment = emptyCommentCreate {
          ticketCommentCreate_body = Just "Wilma Flintstone, steven+wilma@steshaw.org, Awesome Jobs"
        , ticketCommentCreate_public = Just False
        }
      let ticket = TicketCreate {
          ticketCreate_subject = Just "Awesome Jobs — Betterteam trial"
        , ticketCreate_comment = comment
        }
      run subdomain (BS8.pack username) (BS8.pack password) (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

main :: IO ()
main = hspec $ do
  usersSpec
  ticketsSpec
