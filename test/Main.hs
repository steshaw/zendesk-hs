module Main (main) where

import UsersSpec (usersSpec)
import TicketsSpec (ticketsSpec)

import Test.Hspec

main :: IO ()
main = hspec $ do
  usersSpec
  ticketsSpec
