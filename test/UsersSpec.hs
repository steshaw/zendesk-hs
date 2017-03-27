{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UsersSpec (usersSpec) where

import Zendesk
import qualified Zendesk.Internal.MockServer as MockServer

import qualified Data.ByteString.Base64 as Base64
import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

users :: ResponseMatcher
users = [json|
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

basicAuthHeader username password =
  let auth = "Basic " <> Base64.encode (username <> ":" <> password)
  in ("Authorization", auth)

authGet url =
  let emptyBody = ""
  in request "GET" url [basicAuthHeader "fred" "password"] emptyBody

mockUsersSpec :: Spec
mockUsersSpec = with (return MockServer.app) $ do
  describe "GET /users.json" $ do
    it "responds with 200" $ do
      authGet "/users.json" `shouldRespondWith` 200
    it "responds with users" $ do
      authGet "/users.json" `shouldRespondWith` users

zendeskUsersSpec :: Spec
zendeskUsersSpec =
  describe "GET /users.json" $ do
    it "responds okay" $ do
      (subdomain, username, password) <- Zendesk.env
      let users = run subdomain username password Zendesk.getUsers
      let someUsers (Right (Users users)) = length users > 0
      users >>= (`shouldSatisfy` someUsers)

usersSpec :: Spec
usersSpec = do
  describe "MockUsers" mockUsersSpec
  describe "ZendeskUsers" zendeskUsersSpec
