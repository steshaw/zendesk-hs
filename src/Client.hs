module Client where

import Lib (api)

import Servant
import Servant.Client

{-
type API
    = "public"  :> PublicAPI
 :<|> UsersAPI
 :<|> "private" :> BasicAuth "protected-realm" User :> PrivateAPI
 :<|> BasicAuth "protected-realm" User :> TicketsAPI
-}

(getPublic :<|>
 getUsers :<|>
 getPrivate :<|>
 getTickets) = client api
