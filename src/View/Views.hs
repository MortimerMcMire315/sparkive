{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views
    ( homePage
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
                              , Response )

import Control.Monad.IO.Class ( liftIO  )
import Text.Hamlet            ( Html    )

import View.ContentTypes ( MIMEType(CSS, JS, HTML) )
import View.Util         ( nullDirServe
                         , tryQuery
                         , withConn
                         , withConnErrBox
                         , EDBConn                 )
import View.LoginView    ( login                   )
import qualified DB.Query as Query
import qualified View.Template as Template
import qualified Auth.Login as Login

-- |Serve a CSS file.
serveCSS :: ServerPart Response
serveCSS = path $ \(cssRequest :: String) ->
             case cssRequest of
                "styles.css" -> nullDirServe Template.mainStyleSheet CSS
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)

serveJS :: ServerPart Response
serveJS = path $ \(jsRequest :: String) ->
             case jsRequest of
                "create-db-button.js" -> nullDirServe Template.createDBButtonJS JS
                _            -> notFound $ toResponse ("JavaScript file not fouund." :: String)


createDBButton :: EDBConn -> ServerPart Response
createDBButton eitherConn = do
    results <- liftIO $ withConnErrBox eitherConn
        (\conn -> tryQuery conn (Query.createDB "sparkive")
                                (\_ -> return $ Template.genericResultT [["something"]])
        )
    ok $ toResponse results

homePage :: EDBConn -> ServerPart Response
homePage eitherConn =
      liftIO ( withConnErrBox eitherConn
                    (\conn ->
                        tryQuery conn Query.checkDBExists (\exists ->
                            if exists
                            then return $ Template.genericResultT [["something"]]
                            else return Template.createDBButtonT
                                                          )
                    )
      ) >>= (ok . toResponse . Template.homePageT)
