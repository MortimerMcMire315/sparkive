{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views
    ( homePage
    , serveCSS
    , serveJS
    , createDBButton
    ) where

import Debug.Trace (trace)

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
import View.Util         ( provideContext
                         , requireLogin
                         , nullDirServe
                         , tryQuery, withConn
                         , withConnErrBox
                         , SchrodingerConn         )
import DB.Types          ( DBConn                  )

import qualified DB.Query as Query
import qualified View.RenderContext as RC
import qualified View.Template as Template
import qualified Auth.Login as Login

-- |Serve a CSS file from 'View.Template'
serveCSS :: SessionServerPart Response
serveCSS = lift . path $ \(cssRequest :: String) ->
                         case cssRequest of
                            "styles.css" -> nullDirServe Template.mainStyleSheet CSS
                            _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)

-- |Serve a JS file from 'View.Template'
serveJS :: SessionServerPart Response
serveJS = lift . path $ \(jsRequest :: String) ->
             case jsRequest of
                "create-db-button.js" -> nullDirServe Template.createDBButtonJS JS
                _            -> notFound $ toResponse ("JavaScript file not fouund." :: String)

createDBButton :: SchrodingerConn -> SessionServerPart Response
createDBButton eitherConn = do
    results <- withConnErrBox eitherConn
        (\conn -> liftIO $
                    tryQuery conn (Query.createDB "sparkive")
                                  (\_ -> demoQuery conn)
        )
    ok $ toResponse results

demoQuery :: DBConn -> IO Html
demoQuery conn = return $ Template.genericResultT [["something"]]

homePage :: SchrodingerConn -> SessionServerPart Response
homePage sConn = homeContext >>= (ok . toResponse . Template.homePageT)
    where homeContext =
            withConn sConn (return . RC.errorRenderContext)
                           (\conn -> provideContext conn $
                                tryQuery conn Query.checkDBExists
                                   (\dbExists -> if dbExists
                                                 then demoQuery conn
                                                 else return Template.createDBButtonT
                                   )
                           )
