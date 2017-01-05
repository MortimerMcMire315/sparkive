{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views
    ( adminPanel
    , homePage
    , serveCSS
    , serveJS
    , createDBButton
    , login
    ) where

import Happstack.Server       ( notFound
                              , ok
                              , path
                              , toResponse
                              , ServerPart
                              , ServerPartT
                              , Response )

import Control.Monad.IO.Class    ( liftIO  )
import Control.Monad.Trans.Class ( lift    )
import Text.Hamlet               ( Html    )

import Auth.Session      ( SessionServerPart       )
import View.ContentTypes ( MIMEType(CSS, JS, HTML) )
import View.Util         ( requireLogin
                         , nullDirServe
                         , tryQuery, withConn
                         , withConnErrBox, EDBConn )
import View.LoginView    ( login                   )
import qualified DB.Query as Query
import qualified View.Template as Template
import qualified Auth.Login as Login

-- |Serve a CSS file.
serveCSS :: SessionServerPart Response
serveCSS = lift . path $ \(cssRequest :: String) ->
                         case cssRequest of
                            "styles.css" -> nullDirServe Template.mainStyleSheet CSS
                            _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)

serveJS :: SessionServerPart Response
serveJS = lift . path $ \(jsRequest :: String) ->
             case jsRequest of
                "create-db-button.js" -> nullDirServe Template.createDBButtonJS JS
                _            -> notFound $ toResponse ("JavaScript file not fouund." :: String)

createDBButton :: EDBConn -> SessionServerPart Response
createDBButton eitherConn = do
    results <- liftIO $ withConnErrBox eitherConn
        (\conn -> tryQuery conn (Query.createDB "sparkive")
                                (\_ -> return $ Template.genericResultT [["something"]])
        )
    ok $ toResponse results

homePage :: EDBConn -> SessionServerPart Response
homePage eitherConn = ok . toResponse $ Template.homePageT Template.createDBButtonT

adminPanel :: EDBConn -> SessionServerPart Response
adminPanel eitherConn = requireLogin $
    ok $ toResponse Template.adminPanelT
