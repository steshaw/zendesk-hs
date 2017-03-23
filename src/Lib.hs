{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
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

fred = User 3 "Fred" "Flintstone"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , fred
        ]

-- | Public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | private data that needs protection
newtype PrivateData = PrivateData { shh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | A type to wrap our public API.
type PublicAPI = Get '[JSON] [PublicData]

-- | A type to wrap our private API.
type PrivateAPI = Get '[JSON] PrivateData

type UsersAPI = "users" :> Get '[JSON] [User]

-- | our API
type BasicAPI
    = "public"  :> PublicAPI
 :<|> UsersAPI
 :<|> "private" :> BasicAuth "protected-realm" User :> PrivateAPI

type API = BasicAPI
