{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Zendesk.API where

import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

type Id = Int64

type Date = UTCTime

data User = User
  { userId :: Maybe Id
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , userActive :: Maybe Bool
-- Name	Type	Read-only	Mandatory	Comment
-- id	integer	yes	no	Automatically assigned when the user is created
-- email	string	no	no	The user's primary email address
-- name	string	no	yes	The user's name
-- active	boolean	yes	no	false if the user has been deleted
-- alias	string	no	no	An alias displayed to end users
-- chat_only	boolean	yes	no	Whether or not the user is a chat-only agent
-- created_at	date	yes	no	The time the user was created
-- custom_role_id	integer	no	no	A custom role if the user is an agent on the Enterprise plan
-- details	string	no	no	Any details you want to store about the user, such as an address
-- external_id	string	no	no	A unique id you can specify for the user
-- last_login_at	date	yes	no	The last time the user signed in to Zendesk Support
-- locale	string	yes	no	The user's locale
-- locale_id	integer	no	no	The user's language identifier
-- moderator	boolean	no	no	Designates whether the user has forum moderation capabilities
-- notes	string	no	no	Any notes you want to store about the user
-- only_private_comments	boolean	no	no	true if the user can only create private comments
-- organization_id	integer	no	no	The id of the organization the user is associated with
-- default_group_id	integer	no*	no	The id of the user's default group. *Can only be set on create, not on update
-- phone	string	no	no	The user's primary phone number. See Phone Number below
-- photo	Attachment	no	no	The user's profile picture represented as an Attachment object
-- restricted_agent	boolean	no	no	If the agent has any restrictions; false for admins and unrestricted agents, true for other agents
-- role	string	no	no	The user's role. Possible values are "end-user", "agent", or "admin"
-- shared	boolean	yes	no	If the user is shared from a different Zendesk Support instance. Ticket sharing accounts only
-- shared_agent	boolean	yes	no	If the user is a shared agent from a different Zendesk Support instance. Ticket sharing accounts only
-- signature	string	no	no	The user's signature. Only agents and admins can have signatures
-- suspended	boolean	no	no	If the agent is suspended. Tickets from suspended users are also suspended, and these users cannot sign in to the end user portal
-- tags	array	no	no	The user's tags. Only present if your account has user tagging enabled
-- ticket_restriction	string	no	no	Specifies which tickets the user has access to. Possible values are: "organization", "groups", "assigned", "requested", null
-- time_zone	string	no	no	The user's time zone. See Time Zone below
-- two_factor_auth_enabled	boolean	yes	no	If two factor authentication is enabled.
-- updated_at	date	yes	no	The time the user was last updated
-- url	string	yes	no	The user's API url
-- user_fields	hash	no	no	Custom fields for the user
-- verified	boolean	no	no	If the user's identity has been verified or not
  } deriving (Eq, Show, Generic)

aesonOptions fieldLabelModifier = defaultOptions
  { fieldLabelModifier = fieldLabelModifier
  , omitNothingFields = True
  }

toField prefix fieldName = case List.stripPrefix prefix fieldName of
  Just (c : cs) -> Char.toLower c : cs
  _      -> error $ unlines
    [ "Error in toField"
    , "prefix='" <> prefix <> "'"
    , "'fieldName = '" <> fieldName <> "'"
    , "This indicates a programming bug"
    ]

userOptions = aesonOptions (toField "user") -- TODO lowercase first letter

instance ToJSON User where
  toJSON = genericToJSON userOptions

instance FromJSON User where
  parseJSON = genericParseJSON userOptions

newtype Users = Users
  { usersUsers :: [User] }
  deriving (Show, Generic)

usersOptions = aesonOptions (toField "users")

instance ToJSON Users where
  toJSON = genericToJSON usersOptions

instance FromJSON Users where
  parseJSON = genericParseJSON usersOptions

data TicketType = Problem | Incident | Question | Task

data TicketStatus = New | Open | Pending | Hold | Solved | Closed

data TicketPriority = Urgent | High | Normal | Low

-- "requester": { "locale_id": 8, "name": "Pablo", "email": "pablito@example.org" }
data Requester = Requester
  { requesterLocaleId :: Maybe Id
  , requesterName :: Maybe Text
  , requesterEmail :: Maybe Text
  } deriving (Show, Generic)

requesterOptions :: Options
requesterOptions = aesonOptions (toField "requester")

instance ToJSON Requester where
  toJSON = genericToJSON requesterOptions

instance FromJSON Requester where
  parseJSON = genericParseJSON requesterOptions

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/ticket_comments#ticket-comments
--
data TicketCommentCreate = TicketCommentCreate
  { ticketCommentCreateBody :: Maybe Text
  , ticketCommentCreateHtmlBody :: Maybe Text
  , ticketCommentCreatePublic :: Maybe Bool
  , ticketCommentCreateAuthorId :: Maybe Id
  }
  deriving (Show)

consMaybe :: (ToJSON v, KeyValue a) => Text -> Maybe v -> [a] -> [a]
consMaybe fieldName = maybe id ((:) . (fieldName .=))

instance ToJSON TicketCommentCreate where
  toJSON comment = object fields
    where
      conss = consMaybe "body" (ticketCommentCreateBody comment)
            . consMaybe "html_body" (ticketCommentCreateHtmlBody comment)
            . consMaybe "public" (ticketCommentCreatePublic comment)
            . consMaybe "author_id" (ticketCommentCreateAuthorId comment)
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
  { ticketCreateSubject :: Maybe Text
  , ticketCreateComment :: TicketCommentCreate
  , ticketCreateRequester :: Maybe Requester
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
      conss = consMaybe "subject" (ticketCreateSubject ticket)
            . consMaybe "requester" (ticketCreateRequester ticket)
      fields = conss ["comment" .= ticketCreateComment ticket]

instance FromJSON TicketCreate where
  parseJSON = withObject "TicketCreate" $ \wrapper -> do
    ticket <- wrapper .: "ticket"
    subject <- ticket .: "subject"
    comment <- ticket .: "comment"
    requester <- ticket .: "requester"
    return TicketCreate
      { ticketCreateSubject = subject
      , ticketCreateComment = comment
      , ticketCreateRequester = requester
      }

data Ticket = Ticket
  { ticketId :: Maybe Id
  , ticketUrl :: Maybe Text
  , ticketSubject :: Maybe Text
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

newtype TicketCreateResponse = TicketCreateResponse
  { status :: Maybe Int } -- TODO: replace with real result audit/ticket.
  deriving (Eq, Show, Generic)

instance ToJSON TicketCreateResponse
instance FromJSON TicketCreateResponse

type Authed = BasicAuth "protected-realm" User

type GetUsers = "users.json" :> Get '[JSON] Users

type GetTickets = "tickets" :> Get '[JSON] TicketPage

type PostTicket =
  "tickets"
  :> ReqBody '[JSON] TicketCreate
  :> Post '[JSON] TicketCreateResponse

type API
    = Authed :> GetUsers
 :<|> Authed :> GetTickets
 :<|> Authed :> PostTicket

api :: Proxy API
api = Proxy
