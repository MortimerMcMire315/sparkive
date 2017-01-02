module View.Util ( nullDirServe
                 , requireLogin
                 , tryQuery
                 , withConn
                 , withConnErrBox
                 , EDBConn        ) where

import Control.Monad.IO.Class ( liftIO )
import Happstack.Server       ( ok
                              , toResponse
                              , nullDir
                              , askRqEnv
                              , ServerPart
                              , ServerPartT
                              , Response
                              , ToMessage  )
import Text.Hamlet            ( Html       )

import View.ContentTypes ( toResMime
                         , MIMEType           )
import DB.Types          ( DBConn             )
import Auth.Session      ( getToken
                         , SessionServerPart  )
import qualified View.Template as T

type EDBConn = Either String DBConn

-- |Make sure the request is sane (no path segments after *.css or *.js or
--  whatever); if so, serve the file with MIME type "text/css"
nullDirServe :: (ToMessage t) => t -> MIMEType -> ServerPart Response
nullDirServe template mimeT = nullDir >> ok (toResMime template mimeT)

withConn :: (Monad m) => EDBConn -> (String -> m a) -> (DBConn -> m a) -> m a
withConn eitherConn failAction successAction =
    case eitherConn of
        Left err -> failAction err
        Right conn -> successAction conn

withConnErrBox :: EDBConn -> (DBConn -> IO Html) -> IO Html
withConnErrBox eitherConn = withConn eitherConn (return . T.errBoxT)

-- |Attempt to run a SQL query given a 'DBConn', a query function from 'Query',
-- and a function to run on success. The success function uses the results of
-- the query to construct an HTML fragment. On failure, uses 'T.errBoxT'
-- to display an error on the frontend.
tryQuery :: DBConn -> (DBConn -> IO (Either String a)) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- queryF conn
    case eitherErrResults of
        Left err        -> return $ T.errBoxT err
        Right results   -> successAction results

requireLogin :: SessionServerPart Response -> SessionServerPart Response
requireLogin action = do
    maybeToken   <- getToken
    rqData       <- askRqEnv
    let needLogin = "You must be logged in to access this page."
    case maybeToken of
        Nothing    -> liftIO (print rqData) >> (ok . toResponse $ T.loginPageT (Just $ T.errBoxT needLogin))
        --TODO actually check the token
        Just token -> action
