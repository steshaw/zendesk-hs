{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
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

-- | Public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData
instance FromJSON PublicData

-- | private data that needs protection
newtype PrivateData = PrivateData { shh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData
instance FromJSON PrivateData

-- | A type to wrap our public API.
type PublicAPI = Get '[JSON] [PublicData]

-- | A type to wrap our private API.
type PrivateAPI = Get '[JSON] PrivateData

type UsersAPI = "users" :> Get '[JSON] [User]

data Ticket = Ticket
  { ticketId :: Int64
  }
  deriving Show

instance ToJSON Ticket where
  toJSON ticket = object
    [ "id" .= ticketId ticket
    ]

instance FromJSON Ticket where
  parseJSON = withObject "Ticket" $ \ticket -> Ticket
    <$> ticket .: "id"

data TicketPage = TicketPage
  { ticketCount :: Int
  , ticketNextPage :: Maybe Text -- TODO: actually URL.
  , ticketPrevPage :: Maybe Text -- TODO: actually URL.
  , tickets :: [Ticket]
  }
  deriving Show

instance ToJSON TicketPage where
  toJSON ticketPage = object
    [ "count" .= ticketCount ticketPage
    , "next_page" .= ticketNextPage ticketPage
    , "previous_page" .= ticketPrevPage ticketPage
    , "tickets" .= tickets ticketPage
    ]

instance FromJSON TicketPage where
  parseJSON = withObject "TicketPage" $ \ticketPage -> TicketPage
    <$> ticketPage .: "count"
    <*> ticketPage .: "next_page"
    <*> ticketPage .: "previous_page"
    <*> ticketPage .: "tickets"

-- Ticket example
{-
{
    "count": 1,
    "next_page": null,
    "previous_page": null,
    "tickets": [
    {
        "allow_channelback": false,
        "assignee_id": 20546299328,
        "brand_id": 5833508,
        "collaborator_ids": [],
        "created_at": "2017-03-22T05:15:46Z",
        "custom_fields": [],
        "description": "Hi Steven,\n\nEmails, chats, voicemails, and tweets are captured in Zendesk Support as tickets. Start typing above to respond and click Submit to send. To test how an email becomes a ticket, send a message to support@steshaw.zendesk.com.\n\nCurious about what your customers will see when you reply? Check out this video:\nhttps://demos.zendesk.com/hc/en-us/articles/202341799\n",
        "due_at": null,
        "external_id": null,
        "fields": [],
        "forum_topic_id": null,
        "group_id": 39155288,
        "has_incidents": false,
        "id": 1,
        "is_public": true,
        "organization_id": null,
        "priority": "normal",
        "problem_id": null,
        "raw_subject": "Sample ticket: Meet the ticket",
        "recipient": null,
        "requester_id": 20472176027,
        "satisfaction_rating": null,
        "sharing_agreement_ids": [],
        "status": "solved",
        "subject": "Sample ticket: Meet the ticket",
        "submitter_id": 20546299328,
        "tags": [
            "sample",
            "support",
            "zendesk"
        ],
        "type": "incident",
        "updated_at": "2017-03-22T05:50:01Z",
        "url": "https://steshaw.zendesk.com/api/v2/tickets/1.json",
        "via": {
            "channel": "sample_ticket",
            "source": {
                "from": {},
                "rel": null,
                "to": {}
            }
        }
    }
  ]
-}

type TicketsAPI = "tickets" :> Get '[JSON] TicketPage

type API
    = "public"  :> PublicAPI
 :<|> UsersAPI
 :<|> "private" :> BasicAuth "protected-realm" User :> PrivateAPI
 :<|> BasicAuth "protected-realm" User :> TicketsAPI

api :: Proxy API
api = Proxy
