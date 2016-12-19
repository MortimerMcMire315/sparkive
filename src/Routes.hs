{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} --Added to permit the inferred type of nullDirServe in the function serveCSS

module Routes where

import Happstack.Server
import Control.Monad (msum)

import qualified Template as T
import ContentTypes (MIMEType(..))
import qualified ContentTypes as CT
import qualified DBConn as DB
import Control.Monad.Trans.Class (lift)

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
    conn <- lift DB.getConn
    case conn of
        Left e -> error e
        Right c -> do
            results <- lift $ DB.unsafeExampleQuery c
            ok . toResponse $ T.homePage results
