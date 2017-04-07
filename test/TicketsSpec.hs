{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TicketsSpec (ticketsSpec) where

import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Zendesk

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

emptyCommentCreate = TicketCommentCreate
  { ticketCommentCreate_body = Nothing
  , ticketCommentCreate_htmlBody = Nothing
  , ticketCommentCreate_public = Nothing
  , ticketCommentCreate_authorId = Nothing
  }

cannedTicket :: Aeson.Value
cannedTicket = [aesonQQ|
{
  "ticket": {
    "subject": "Awesome Jobs — Betterteam trial",
      "comment": {
        "body": "Wilma Flintstone, steven+wilma@steshaw.org, Awesome Jobs",
        "public": false
    },
    "requester": {
      "email": "steven+wilma@steshaw.org",
      "name": "Wilma Flintstone"
    }
  }
}
|]

ticketsSpec :: Spec
ticketsSpec =
  describe "POST /tickets.json" $ do

    it "responds okay with a subject" $ do
      (subdomain, username, password) <- Zendesk.env
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate (Just "A subject") comment Nothing
      run subdomain username password (`createTicket` ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "responds okay without a subject" $ do
      (subdomain, username, password) <- Zendesk.env
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate Nothing comment Nothing
      run subdomain username password (`createTicket` ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "can create a private ticket (with private comment)" $ do
      (subdomain, username, password) <- Zendesk.env
      let comment = emptyCommentCreate {
          ticketCommentCreate_body = Just "Wilma Flintstone, steven+wilma@steshaw.org, Awesome Jobs"
        , ticketCommentCreate_public = Just False
        }
      let ticket = TicketCreate {
          ticketCreate_subject = Just "Awesome Jobs — Betterteam trial"
        , ticketCreate_comment = comment
        , ticketCreate_requester = Just Requester {
              requester_localeId = Nothing
            , requester_name = Just "Wilma Flintstone"
            , requester_email = Just "steven+wilma@steshaw.org"
          }
        }
      Aeson.toJSON ticket `shouldBe` cannedTicket
      run subdomain username password (`createTicket` ticket)
       `shouldReturn` Right (TicketCreateResponse Nothing)
