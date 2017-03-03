module Routes
    ( entryPoint
    , routes
    ) where

import Happstack.Server
import Control.Monad          ( msum )
import Control.Monad.IO.Class ( liftIO )

import DB.Conn      ( getConn )
import DB.Types     ( DBConn  )
import Auth.Session ( SessionServerPart )
import qualified View.Views as Views
import qualified View.LoginView as Login
import qualified View.Template as T
import qualified View.RenderContext as RC

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- |Happstack routing function.
--  /css/* -> 'Views.serveCSS'
--  /      -> 'Views.homePage'
entryPoint :: SessionServerPart Response
entryPoint = do
    eitherConn <- liftIO getConn
    case eitherConn of
        Left err -> msum [
                           baseRoutes
                         , ok . toResponse . T.homePageT $ RC.errorRenderContext err
                         ]
        Right conn -> routes conn

baseRoutes :: SessionServerPart Response
baseRoutes = msum [
                    dir  "css"                   Views.serveCSS
                  , dir  "js"                    Views.serveJS
                  ]

routes :: DBConn -> SessionServerPart Response
routes conn = do
    decodeBody myPolicy
    msum [
           baseRoutes
         , dir  "css"                   Views.serveCSS
         , dir  "js"                    Views.serveJS
         , dirs "ajax/create-db"      $ Views.createDBButton conn
         , dir  "login"               $ Login.login conn
         , dir  "admin-panel"         $ Login.adminPanel conn
         , nullDir                   >> Views.homePage conn
         ]
