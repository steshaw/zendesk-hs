{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UsersSpec (usersSpec) where

import Zendesk
import qualified Zendesk.Internal.MockServer as MockServer

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.String (IsString)
import Data.Monoid ((<>))
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

usersJSON :: ResponseMatcher
usersJSON = [json|
{ "users":
    [ { "id": 1
      , "name": "Issac Newton"
      }
    , { "id": 2
      , "name": "Albert Einstein"
      }
    , { "id": 3
      , "email": "fred.flintstone@gmail.com"
      , "name": "Fred Flintstone"
      }
    ]
}
|]

basicAuthHeader :: IsString header => ByteString -> ByteString -> (header, ByteString)
basicAuthHeader username password =
  let auth = "Basic " <> Base64.encode (username <> ":" <> password)
  in ("Authorization", auth)

authGet :: ByteString -> WaiSession SResponse
authGet url =
  let emptyBody = ""
  in request "GET" url [basicAuthHeader "fred" "password"] emptyBody

mockUsersSpec :: Spec
mockUsersSpec = with (return MockServer.app) $
  describe "GET /users.json" $ do
    it "responds with 200" $
      authGet "/users.json" `shouldRespondWith` 200
    it "responds with users" $
      authGet "/users.json" `shouldRespondWith` usersJSON

zendeskUsersSpec :: Spec
zendeskUsersSpec =
  describe "GET /users.json" $
    it "responds okay" $ do
      (subdomain, username, password) <- Zendesk.env
      let users = run subdomain username password Zendesk.getUsers
          someUsers (Right (Users users')) = not (null users')
          someUsers _                     = False
      users >>= (`shouldSatisfy` someUsers)

usersSpec :: Spec
usersSpec = do
  describe "MockUsers" mockUsersSpec
  describe "ZendeskUsers" zendeskUsersSpec
