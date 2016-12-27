module Routes
    ( routes
    ) where

import Happstack.Server
import Control.Monad (msum)

import DB.Types (DBConn)
import qualified View.Views as Views

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- |Happstack routing function.
--  /css/* -> 'Views.serveCSS'
--  /      -> 'Views.homePage'
routes :: Either String DBConn -> ServerPart Response
routes conn = do
    decodeBody myPolicy
    msum [
           dir  "css"                   Views.serveCSS
         , dir  "js"                    Views.serveJS
         , dirs "ajax/create-db"        (Views.createDBButton conn)
         , dir  "login"                 (Views.login conn)
         , nullDir                   >> Views.homePage conn
         ]

