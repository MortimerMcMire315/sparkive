{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views
    ( homePage
    , serveCSS
    , serveJS
    , createDBButton
    ) where

import Happstack.Server (ok, toResponse, ServerPart, Response, path, notFound, nullDir)

import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Catch (catches)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (Html)

import qualified DB.Conn as DB
import qualified DB.Query as Query
import qualified Exception.Handler as E
import qualified View.Template as Template
import View.ContentTypes (MIMEType(CSS, JS, HTML), toResMime)

-- Make sure the request is sane (no path segments after *.css or *.js or whatever);
-- if so, serve the file with MIME type "text/css"
--nullDirServe :: ServerPart Response
nullDirServe template mimeT = nullDir >> ok (toResMime template mimeT)

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


createDBButton :: ServerPart Response
createDBButton = do
    results <- liftIO $ withConnErrBox
        (\conn -> tryQuery conn (Query.createDB "sparkive")
                                --const allows us to ignore the result
                                (const $ tryQuery conn DB.exampleQuery (return . Template.genericResultT))
        )
    ok $ toResponse results

-- |Attempt to run a SQL query given a 'Connection', a query function, and a
-- function to run on success. The success function uses the results of the
-- query to construct an HTML fragment. On failure, uses 'Template.errBoxT' to
-- display an error on the frontend.
tryQuery :: Connection -> (Connection -> IO a) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- catches
                            (Right <$> queryF conn)
                            (E.sqlErrorHandlers ++ [E.handleReadFileException])
    case eitherErrResults of
        Left err        -> return $ Template.errBoxT err
        Right results   -> successAction results


-- |Attempt to open a database connection and perform an IO action (returning
-- 'Html') with the connection. The first argument is a function which returns
-- Html based on a given error string, and the second argument is a function
-- which returns Html with a DB connection. An idiomatic way to use this
-- function is as follows:
--
-- @
-- resultHtml <- withConn (\err -> return $ Template.errBoxT err)
--                        (\conn ->
--                             tryQuery conn DB.someQuery (\results ->
--                                   return $ Template.genericResultT results
--                                                        )
--                        )
-- @
withConn :: (String -> IO Html) -> (Connection -> IO Html) -> IO Html
withConn failAction successAction = do
    eitherErrConn <- catches
                        (fmap Right DB.getConn)
                           [
                             E.handleConfigParseException
                           , E.handleSQLConnectionException
                           , E.handleInvalidPortException
                           ]
    case eitherErrConn of
        Left err   -> failAction err
        Right conn -> successAction conn

-- |Attempt to open a DB connection. If it succeeds, run the given IO action,
-- which returns HTML to display on success. If the connection fails, return
-- the HTML for an error box containing a string to display to the user.
withConnErrBox :: (Connection -> IO Html) -> IO Html
withConnErrBox = withConn (return . Template.errBoxT)

{-- The homepage is really just a sandbox for now. --}
homePage :: ServerPart Response
homePage = do
    toInsert <- liftIO $ withConnErrBox
        (\conn -> do
            exists <- Query.checkDBExists conn
            if exists
            then tryQuery conn DB.exampleQuery (return . Template.genericResultT)
            else return Template.createDBButtonT
        )
    ok . toResponse $ Template.homePageT toInsert
