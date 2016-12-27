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

import Happstack.Server

import Control.Monad (msum)
import Control.Monad.Catch (catches)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (Html)

import DB.Types (DBConn)
import qualified DB.Conn as DB
import qualified DB.Query as Query
import qualified Exception.Handler as E
import qualified View.Template as Template
import qualified Auth.Login as Login
import View.ContentTypes (MIMEType(CSS, JS, HTML), toResMime)

type EDBConn = Either String DBConn

-- Make sure the request is sane (no path segments after *.css or *.js or whatever);
-- if so, serve the file with MIME type "text/css"
--nullDirServe :: ServerPart Response
nullDirServe template mimeT = nullDir >> ok (toResMime template mimeT)

withConn :: EDBConn -> (String -> IO a) -> (DBConn -> IO a) -> IO a
withConn eitherConn failAction successAction =
    case eitherConn of
        Left err -> failAction err
        Right conn -> successAction conn

withConnErrBox :: EDBConn -> (DBConn -> IO Html) -> IO Html
withConnErrBox eitherConn = withConn eitherConn (return . Template.errBoxT)

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
                                --const allows us to ignore the result
                                --(const $ tryQuery conn (show $ Query.getPassHash "sayoder") (return . Template.genericResultT))
                                (\_ -> return $ Template.genericResultT [["something"]])
        )
    ok $ toResponse results

-- |Attempt to run a SQL query given a 'DBConn', a query function, and a
-- function to run on success. The success function uses the results of the
-- query to construct an HTML fragment. On failure, uses 'Template.errBoxT' to
-- display an error on the frontend.

tryQuery :: DBConn -> (DBConn -> IO a) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- catches
                            (Right <$> queryF conn)
                            (E.sqlErrorHandlers ++ [E.handleReadFileException])
    case eitherErrResults of
        Left err        -> return $ Template.errBoxT err
        Right results   -> successAction results


login :: EDBConn -> ServerPart Response
login eitherconn = msum [ method [GET,HEAD] >> ok (toResponse Template.loginPageT)
--                      , method POST >> doLogin
                        ]

--testCreateAccount :: String -> String -> IO ()
--testCreateAccount username pass =
--    withConn print
--             (\conn -> do
--                 res <- Login.storeUser username pass conn
--                 case res of
--                     Left err -> print err
--                     Right _ -> putStrLn "Success!"
--             )
--
--testCheckPass :: String -> String -> IO ()
--testCheckPass username pass =
--    withConn print
--             (\conn -> do
--                 res <- Login.isCorrectPass username pass conn
--                 case res of
--                    Left err -> print err
--                    Right collision -> print collision
--             )

{-- The homepage is really just a sandbox for now. --}
homePage :: EDBConn -> ServerPart Response
homePage eitherConn = do
    toInsert <- liftIO $ withConnErrBox eitherConn
        (\conn -> do
            exists <- Query.checkDBExists conn
            if exists
            then return $ Template.genericResultT [["something"]]
            else return Template.createDBButtonT
        )
    ok . toResponse $ Template.homePageT toInsert
