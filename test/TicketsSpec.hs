{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TicketsSpec (ticketsSpec) where

import Zendesk

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS8
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

emptyCommentCreate = TicketCommentCreate
  { ticketCommentCreate_body = Nothing
  , ticketCommentCreate_htmlBody = Nothing
  , ticketCommentCreate_public = Nothing
  , ticketCommentCreate_authorId = Nothing
  }

zendeskEnv = do
  subdomain <- Env.getEnv "ZENDESK_SUBDOMAIN"
  username <- Env.getEnv "ZENDESK_USERNAME"
  password <- Env.getEnv "ZENDESK_PASSWORD"
  return (subdomain, BS8.pack username, BS8.pack password)

ticketsSpec :: Spec
ticketsSpec =
  describe "POST /tickets.json" $ do

    it "responds okay with a subject" $ do
      (subdomain, username, password) <- zendeskEnv
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate (Just "A subject") comment
      run subdomain username password (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "responds okay without a subject" $ do
      (subdomain, username, password) <- zendeskEnv
      let comment = emptyCommentCreate { ticketCommentCreate_body = Just "A body" }
      let ticket = TicketCreate Nothing comment
      run subdomain username password (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)

    it "can create a private ticket (with private comment)" $ do
      (subdomain, username, password) <- zendeskEnv
      let comment = emptyCommentCreate {
          ticketCommentCreate_body = Just "Wilma Flintstone, steven+wilma@steshaw.org, Awesome Jobs"
        , ticketCommentCreate_public = Just False
        }
      let ticket = TicketCreate {
          ticketCreate_subject = Just "Awesome Jobs — Betterteam trial"
        , ticketCreate_comment = comment
        }
      run subdomain username password (\auth -> createTicket auth ticket)
        `shouldReturn` Right (TicketCreateResponse Nothing)
