module Routes
    ( routes
    ) where

import Happstack.Server
import Control.Monad ( msum )

import DB.Types     ( DBConn            )
import Auth.Session ( SessionServerPart )
import qualified View.Views as Views
import qualified View.LoginView as Login

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- |Happstack routing function.
--  /css/* -> 'Views.serveCSS'
--  /      -> 'Views.homePage'
routes :: Either String DBConn -> SessionServerPart Response
routes conn = do
    decodeBody myPolicy
    msum [
           dir  "css"                   Views.serveCSS
         , dir  "js"                    Views.serveJS
         , dirs "ajax/create-db"      $ Views.createDBButton conn
         , dir  "login"               $ Login.login conn
         , dir  "admin-panel"         $ Login.adminPanel conn
         , nullDir                   >> Views.homePage conn
         ]
