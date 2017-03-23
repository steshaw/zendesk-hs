{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
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

type Id = Int64

type Date = UTCTime

-- | A type to wrap our private API.
type PrivateAPI = Get '[JSON] PrivateData

type UsersAPI = "users" :> Get '[JSON] [User]

data TicketType = Problem | Incident | Question | Task

data TicketStatus = New | Open | Pending | Hold | Solved | Closed

data TicketPriority = Urgent | High | Normal | Low

data TicketCommentCreate = TicketCommentCreate
  { tcc_body :: Maybe Text
  , tcc_htmlBody :: Maybe Text
  , tcc_public :: Maybe Bool
  , tcc_authorId :: Maybe Id
  }
  deriving (Show)

instance ToJSON TicketCommentCreate where
  toJSON comment = object
    [ "body" .= tcc_body comment
    , "html_body" .= tcc_htmlBody comment
    , "public" .= tcc_public comment
    , "author_id" .= tcc_authorId comment
    ]

instance FromJSON TicketCommentCreate where
  parseJSON = withObject "TicketCommentCreate" $ \comment -> TicketCommentCreate
    <$> comment .: "body"
    <*> comment .: "html_body"
    <*> comment .: "public"
    <*> comment .: "author_id"

data TicketCommentType = Comment | VoiceComment

data TicketComment = TicketComment
  { ticketCommentId :: Id
  , body :: Text
  , htmlBody :: Text
  , plainBody :: Text
  , public :: Bool
  , authorId :: Id
-- attachments	array	yes	Attachments, if any. See Attachment
-- via	object	yes	How the comment was created. See Via Object
-- metadata	object	yes	System information (web client, IP address, etc.)
  , createdAt :: Date
  }

data Ticket = Ticket
  { ticketId :: Maybe Id
  , subject :: Text -- XXX: Probably should be optional.
  , comment :: TicketCommentCreate
-- comment	Required. A comment object that describes the problem, incident, question, or task. See Ticket comments
-- requester_id	The numeric ID of the user asking for support through the ticket
-- submitter_id	The numeric ID of the user submitting the ticket
-- assignee_id	The numeric ID of the agent to assign the ticket to
-- group_id	The numeric ID of the group to assign the ticket to
  }
  deriving Show

instance ToJSON Ticket where
  toJSON ticket = object
    [ "id" .= ticketId ticket
    , "subject" .= subject ticket
    , "comment" .= comment ticket
    ]

instance FromJSON Ticket where
  parseJSON = withObject "Ticket" $ \ticket -> Ticket
    <$> ticket .: "id"
    <*> ticket .: "subject"
    <*> ticket .: "comment"

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
