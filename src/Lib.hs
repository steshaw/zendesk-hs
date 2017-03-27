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
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type Id = Int64

type Date = UTCTime

type UsersAPI = "users" :> Get '[JSON] [User]

data TicketType = Problem | Incident | Question | Task

data TicketStatus = New | Open | Pending | Hold | Solved | Closed

data TicketPriority = Urgent | High | Normal | Low

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/ticket_comments#ticket-comments
--
data TicketCommentCreate = TicketCommentCreate
  { ticketCommentCreate_body :: Maybe Text
  , ticketCommentCreate_htmlBody :: Maybe Text
  , ticketCommentCreate_public :: Maybe Bool
  , ticketCommentCreate_authorId :: Maybe Id
  }
  deriving (Show)

consMaybe fieldName = maybe id ((:) . (fieldName .=))

instance ToJSON TicketCommentCreate where
  toJSON comment = object fields
    where
      conss = consMaybe "body" (ticketCommentCreate_body comment)
            . consMaybe "html_body" (ticketCommentCreate_htmlBody comment)
            . consMaybe "public" (ticketCommentCreate_public comment)
            . consMaybe "author_id" (ticketCommentCreate_authorId comment)
      fields = conss []

instance FromJSON TicketCommentCreate where
  parseJSON = withObject "TicketCommentCreate" $ \comment -> TicketCommentCreate
    <$> comment .:? "body"
    <*> comment .:? "html_body"
    <*> comment .:? "public"
    <*> comment .:? "author_id"

data TicketCommentType = Comment | VoiceComment

data TicketComment = TicketComment
  { ticketCommentId :: Id
  , body :: Text
  , htmlBody :: Text
  , plainBody :: Text
  , public :: Bool
  , authorId :: Id
  , createdAt :: Date
-- Name	Type	Read-only	Comment
-- id	integer	yes	Automatically assigned when the comment is created
-- type	string	yes	Comment or VoiceComment
-- body	string	no	The comment string
-- html_body	string	no	The comment formatted as HTML
-- plain_body	string	yes	The comment as plain text
-- public	boolean	no	true if a public comment; false if an internal note. The initial value set on ticket creation persists for any additional comment unless you change it
-- author_id	integer	no	The id of the comment author
-- attachments	array	yes	Attachments, if any. See Attachment
-- via	object	yes	How the comment was created. See Via Object
-- metadata	object	yes	System information (web client, IP address, etc.)
-- created_at	date	yes	The time the comment was created
  }

data TicketCreate = TicketCreate
  { ticketCreate_subject :: Maybe Text
  , ticketCreate_comment :: TicketCommentCreate
-- requester_id	The numeric ID of the user asking for support through the ticket
-- submitter_id	The numeric ID of the user submitting the ticket
-- assignee_id	The numeric ID of the agent to assign the ticket to
-- group_id	The numeric ID of the group to assign the ticket to
  }

instance ToJSON TicketCreate where
  toJSON ticket = object
    [ "ticket" .= object fields
      -- [ "subject" .= ticketCreateSubject ticket
      -- , "comment" .= ticketCreateComment ticket
      -- ]
    ]
    where
      conss = consMaybe "subject" (ticketCreate_subject ticket)
      fields = conss ["comment" .= (ticketCreate_comment ticket)]

instance FromJSON TicketCreate where
  parseJSON = withObject "TicketCreate" $ \wrapper -> do
    ticket <- wrapper .: "ticket"
    subject <- ticket .: "subject"
    comment <- ticket .: "comment"
    return $ TicketCreate
      { ticketCreate_subject = subject
      , ticketCreate_comment = comment
      }

data Ticket = Ticket
  { ticketId :: Maybe Id
  , ticketUrl :: Maybe Text -- XXX: URL
  , ticketSubject :: Maybe Text -- XXX: Probably should be optional.
  , ticketDescription :: Maybe Text

--   Name	Type	Read-only	Mandatory	Comment
-- id	integer	yes	no	Automatically assigned when creating tickets
-- url	string	yes	no	The API url of this ticket
-- external_id	string	no	no	An id you can use to link Zendesk Support tickets to local records
-- type	string	no	no	The type of this ticket, i.e. "problem", "incident", "question" or "task"
-- subject	string	no	no	The value of the subject field for this ticket
-- raw_subject	string	no	no	The dynamic content placeholder, if present, or the "subject" value, if not. See Dynamic Content
-- description	string	yes	no	The first comment on the ticket
-- priority	string	no	no	Priority, defines the urgency with which the ticket should be addressed: "urgent", "high", "normal", "low"
-- status	string	no	no	The state of the ticket, "new", "open", "pending", "hold", "solved", "closed"
-- recipient	string	no	no	The original recipient e-mail address of the ticket
-- requester_id	integer	no	yes	The user who requested this ticket
-- submitter_id	integer	no	no	The user who submitted the ticket; The submitter always becomes the author of the first comment on the ticket.
-- assignee_id	integer	no	no	What agent is currently assigned to the ticket
-- organization_id	integer	no	no	The organization of the requester. You can only specify the ID of an organization associated with the requester. See Organization Memberships
-- group_id	integer	no	no	The group this ticket is assigned to
-- collaborator_ids	array	no	no	Who are currently CC'ed on the ticket
-- forum_topic_id	integer	no	no	The topic this ticket originated from, if any
-- problem_id	integer	no	no	The problem this incident is linked to, if any
-- has_incidents	boolean	yes	no	Is true of this ticket has been marked as a problem, false otherwise
-- due_at	date	no	no	If this is a ticket of type "task" it has a due date. Due date format uses ISO 8601 format.
-- tags	array	no	no	The array of tags applied to this ticket
-- via	Via	yes	no	This object explains how the ticket was created
-- custom_fields	array	no	no	The custom fields of the ticket
-- satisfaction_rating	object	yes	no	The satisfaction rating of the ticket, if it exists, or the state of satisfaction, 'offered' or 'unoffered'
-- sharing_agreement_ids	array	yes	no	The ids of the sharing agreements used for this ticket
-- followup_ids	array	yes	no	The ids of the followups created from this ticket - only applicable for closed tickets
-- ticket_form_id	integer	no	no	The id of the ticket form to render for this ticket - only applicable for enterprise accounts
-- brand_id	integer	no	no	The id of the brand this ticket is associated with - only applicable for enterprise accounts
-- allow_channelback	boolean	yes	no	Is false if channelback is disabled, true otherwise - only applicable for channels framework ticket
-- is_public	boolean	yes	no	Is true if any comments are public, false otherwise
-- created_at	date	yes	no	When this record was created
-- updated_at	date	yes	no	When this record last got updated
  }
  deriving Show

instance ToJSON Ticket where
  toJSON ticket = object
    [ "id" .= ticketId ticket
    , "url" .= ticketUrl ticket
    , "subject" .= ticketSubject ticket
    , "description" .= ticketDescription ticket
    ]

instance FromJSON Ticket where
  -- XXX: Probably change this to record syntax (instead of applicative).
  parseJSON = withObject "Ticket" $ \ticket -> Ticket
    <$> ticket .: "id"
    <*> ticket .: "url"
    <*> ticket .: "subject"
    <*> ticket .: "description"

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

data TicketCreateResponse = TicketCreateResponse
  { status :: Maybe Int } -- XXX: replace with real result audit/ticket.
  deriving (Eq, Show, Generic)

instance ToJSON TicketCreateResponse
instance FromJSON TicketCreateResponse

type Authed = BasicAuth "protected-realm" User

type GetTickets = "tickets.json" :> Get '[JSON] TicketPage

type PostTicket =
  "tickets.json"
  :> ReqBody '[JSON] TicketCreate
  :> Post '[JSON] TicketCreateResponse

type API
    = UsersAPI
 :<|> Authed :> GetTickets
 :<|> Authed :> PostTicket

api :: Proxy API
api = Proxy
