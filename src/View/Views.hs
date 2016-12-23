{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module View.Views 
    ( homePage
    , serveCSS
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
import qualified View.ContentTypes as CT

-- |Serve a CSS file.
serveCSS :: ServerPart Response
serveCSS = path $ \(cssRequest :: String) -> 
             case cssRequest of
                "styles.css" -> nullDirServe Template.mainStyleSheet
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)
                -- Make sure the request is sane (no path segments after *.css);
                -- if so, serve the file with MIME type "text/css"
                where nullDirServe template = nullDir >> (ok $ (CT.toResMime template CT.CSS))


-- |Attempt to run a SQL query given a 'Connection', a query function, and a
-- function to run on success. The success function uses the results of the
-- query to construct an HTML fragment. On failure, uses 'Template.errBoxT' to
-- display an error on the frontend.
tryQuery :: Connection -> (Connection -> IO a) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- catches
                            (fmap Right $ queryF conn)
                            [E.handleSQLError]
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
withConnErrBox = withConn (\err -> return $ Template.errBoxT err)

{-- The homepage is really just a sandbox for now. --}
homePage :: ServerPart Response
homePage = do
    toInsert <- liftIO $ withConnErrBox 
        (\conn -> do
            --Query.createDB conn "sparkive"
            --return $ Template.errBoxT "Not really an error."
            tryQuery conn DB.exampleQuery (\results ->
                  return $ Template.genericResultT results
                                          )
        )
    ok . toResponse $ Template.homePageT toInsert
