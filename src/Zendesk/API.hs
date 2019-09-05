{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Zendesk.API where

import Data.Aeson
import Data.Aeson.Types (camelTo2)
import Data.Int (Int64)
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
  }
  deriving (Show, Eq, Generic)

aesonOptions :: (String -> String) -> Options
aesonOptions fieldModifier = defaultOptions
  { fieldLabelModifier = fieldModifier
  , omitNothingFields = True
  }

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

toField :: String -> String -> String
toField prefix fieldName = case List.stripPrefix prefix fieldName of
  Just s -> camelToSnake s
  _      -> error $ unlines
    [ "Error in toField"
    , "prefix='" <> prefix <> "'"
    , "'fieldName = '" <> fieldName <> "'"
    , "This indicates a programming bug"
    ]

userOptions :: Options
userOptions = aesonOptions (toField "user")

instance ToJSON User where
  toJSON = genericToJSON userOptions

instance FromJSON User where
  parseJSON = genericParseJSON userOptions

newtype Users = Users
  { usersUsers :: [User] }
  deriving (Show, Eq, Generic)

usersOptions :: Options
usersOptions = aesonOptions (toField "users")

instance ToJSON Users where
  toJSON = genericToJSON usersOptions

instance FromJSON Users where
  parseJSON = genericParseJSON usersOptions

data TicketType = Problem | Incident | Question | Task
  deriving (Show, Eq, Generic)

ticketTypeOptions :: Options
ticketTypeOptions = defaultOptions
  { constructorTagModifier = camelToSnake
  }

instance ToJSON TicketType where
  toJSON = genericToJSON ticketTypeOptions

instance FromJSON TicketType where
  parseJSON = genericParseJSON ticketTypeOptions

data TicketStatus = New | Open | Pending | Hold | Solved | Closed

data TicketPriority = Urgent | High | Normal | Low

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/tickets#creating-a-ticket-with-a-new-requester
--
data Requester = Requester
  { requesterLocaleId :: Maybe Id
  , requesterName :: Maybe Text
  , requesterEmail :: Maybe Text
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

ticketCommentCreateOptions :: Options
ticketCommentCreateOptions = aesonOptions (toField "ticketCommentCreate")

instance ToJSON TicketCommentCreate where
  toJSON = genericToJSON ticketCommentCreateOptions

instance FromJSON TicketCommentCreate where
  parseJSON = genericParseJSON ticketCommentCreateOptions

data TicketCommentType = Comment | VoiceComment

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/ticket_comments#json-format
--
data TicketComment = TicketComment
  { ticketCommentId :: Id
  , body :: Text
  , htmlBody :: Text
  , plainBody :: Text
  , public :: Bool
  , authorId :: Id
  , createdAt :: Date
  }

data CustomField = CustomField
  { customFieldId :: Integer
  , customFieldValue :: Data.Aeson.Value
  }
  deriving (Show, Eq, Generic)

customFieldOptions :: Options
customFieldOptions = aesonOptions (toField "customField")

instance ToJSON CustomField where
  toJSON = genericToJSON customFieldOptions

instance FromJSON CustomField where
  parseJSON = genericParseJSON customFieldOptions

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/tickets#create-ticket
-- Particularly https://developer.zendesk.com/rest_api/docs/core/tickets#request-parameters
--
data TicketCreate = TicketCreate
  { ticketCreateSubject :: Maybe Text
  , ticketCreateComment :: TicketCommentCreate
  , ticketCreateRequester :: Maybe Requester
  , ticketCreateTags :: Maybe [Text]
  , ticketCreateCustomFields :: [CustomField]
  , ticketCreateType :: Maybe TicketType
  , ticketCreateDueAt :: Maybe Date
  }
  deriving (Show, Eq, Generic)

ticketCreateOptions :: Options
ticketCreateOptions = aesonOptions (toField "ticketCreate")

instance ToJSON TicketCreate where
  toJSON ticket = object [ "ticket" .= genericToJSON ticketCreateOptions ticket ]

instance FromJSON TicketCreate where
  parseJSON = withObject "TicketCreate" $ \wrapper -> do
    ticket <- wrapper .: "ticket"
    genericParseJSON ticketCreateOptions ticket

-- |
-- See https://developer.zendesk.com/rest_api/docs/core/tickets#json-format
--
data Ticket = Ticket
  { ticketId :: Maybe Id
  , ticketUrl :: Maybe Text
  , ticketSubject :: Maybe Text
  , ticketDescription :: Maybe Text
  , ticketTags :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

ticketOptions :: Options
ticketOptions = aesonOptions (toField "ticket")

instance ToJSON Ticket where
  toJSON = genericToJSON ticketOptions

instance FromJSON Ticket where
  parseJSON = genericParseJSON ticketOptions

data TicketPage = TicketPage
  { ticketPageCount :: Int
  , ticketPageNextPage :: Maybe Text -- TODO: actually URL.
  , ticketPagePrevPage :: Maybe Text -- TODO: actually URL.
  , ticketPageTickets :: [Ticket]
  }
  deriving (Show, Eq, Generic)

ticketPageOptions :: Options
ticketPageOptions = aesonOptions (toField "ticketPage")

instance ToJSON TicketPage where
  toJSON = genericToJSON ticketPageOptions

instance FromJSON TicketPage where
  parseJSON = genericParseJSON ticketPageOptions

newtype TicketCreateResponse = TicketCreateResponse
  { status :: Maybe Int } -- TODO: replace with real result audit/ticket.
  deriving (Show, Eq, Generic)

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
