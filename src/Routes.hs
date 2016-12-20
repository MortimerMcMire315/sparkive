{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Happstack.Server
import Control.Monad (msum)

import ContentTypes (MIMEType(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catch, handle, catches)
import Database.PostgreSQL.Simple (query_)

import qualified Template as T
import qualified ContentTypes as CT
import qualified DBConn as DB
import qualified Exceptions as E

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

routes :: ServerPart Response
routes = do
    decodeBody myPolicy
    msum [ 
           dir "css"      $ serveCSS
         , nullDir       >> homePage
         ]

{-- Serve a CSS file from a finite possibilities --}
serveCSS :: ServerPart Response
serveCSS = path $ \(cssRequest :: String) -> 
             case cssRequest of
                "styles.css" -> nullDirServe T.mainStyleSheet
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)
                -- Make sure the request is sane (no path segments after *.css);
                -- if so, serve the file with MIME type "text/css"
                where nullDirServe template = nullDir >> (ok $ (CT.toResMime template CSS))

homePage :: ServerPart Response
homePage = do
    eitherConn <- liftIO $ catches (fmap Right DB.getConn) [E.handleConfigParseException, E.handleSqlConnectionException]
    case eitherConn of
        Left err -> ok . toResponse $ T.homePageT [[]] err
        Right conn -> do results <- liftIO $ DB.exampleQuery conn
                         ok . toResponse $ T.homePageT results ""
