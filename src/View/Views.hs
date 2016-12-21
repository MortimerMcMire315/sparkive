{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views where

import Happstack.Server (ok, toResponse, ServerPart, Response, path, notFound, nullDir)

import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Catch (catches)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (Html)

import qualified DB.Conn as DB
import qualified Exception.Handler as E
import qualified View.Template as Template
import qualified View.ContentTypes as CT

{-- Serve a CSS file... finite possibilities using "case" for now. --}
serveCSS :: ServerPart Response
serveCSS = path $ \(cssRequest :: String) -> 
             case cssRequest of
                "styles.css" -> nullDirServe Template.mainStyleSheet
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)
                -- Make sure the request is sane (no path segments after *.css);
                -- if so, serve the file with MIME type "text/css"
                where nullDirServe template = nullDir >> (ok $ (CT.toResMime template CT.CSS))


{-- DB Queries --}
tryQuery :: Connection -> (Connection -> IO a) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- catches
                            (fmap Right $ queryF conn)
                            [E.handleSQLError]
    case eitherErrResults of
        Left err        -> return $ Template.errBoxT err
        Right results   -> successAction results


-- | Attempt to open a DB connection. If it succeeds, run the given IO action,
--   which returns HTML to display on success. If the connection fails, return 
--   the HTML for an error box containing a string to display to the user.
withConn :: (Connection -> IO Html) -> IO Html
withConn successAction = do
    eitherErrConn <- catches 
                        (fmap Right DB.getConn) 
                           [ 
                              E.handleConfigParseException 
                              , E.handleSQLConnectionException
                              , E.handleInvalidPortException 
                           ]
    case eitherErrConn of
        Left err   -> return $ Template.errBoxT err
        Right conn -> successAction conn

{-- The homepage is really just a sandbox for now. --}
homePage :: ServerPart Response
homePage = do
    toInsert <- liftIO $ withConn 
        (\conn -> 
             tryQuery conn DB.exampleQuery (\results ->
                   return $ Template.genericResultT results
                                           )
        )
    ok . toResponse $ Template.homePageT toInsert
