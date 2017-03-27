{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UsersSpec (usersSpec) where

import qualified Zendesk.Internal.MockServer as MockServer

import qualified Data.ByteString.Char8 as BS8
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)

users :: ResponseMatcher
users = [json|
  [
    {
      "userId": 1,
      "userFirstName":"Isaac",
      "userLastName":"Newton"
    },
    {
      "userId": 2,
      "userFirstName": "Albert",
      "userLastName": "Einstein"
    },
    {
      "userId": 3,
      "userFirstName": "Fred",
      "userLastName": "Flintstone"
    }
  ]
|]

usersSpec :: Spec
usersSpec = with (return MockServer.app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            get "/users" `shouldRespondWith` users
