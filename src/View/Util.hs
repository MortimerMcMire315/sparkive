module View.Util ( getBaseContext
                 , isLoggedIn
                 , nullDirServe
                 , provideContext
                 , requireLogin
                 , respondWithErr
                 , tryQuery
                 , withConn
                 , withConnErrBox
                 , SchrodingerConn ) where

import Control.Monad.IO.Class ( liftIO )
import Happstack.Server       ( ok
                              , toResponse
                              , nullDir
                              , askRq
                              , rqUri
                              , ServerPart
                              , ServerPartT
                              , Response
                              , ToMessage  )
import Text.Hamlet            ( Html       )
import Data.Maybe             ( isJust     )

import View.ContentTypes ( toResMime
                         , MIMEType          )
import DB.Query          ( verifySessToken   )
import DB.Types          ( DBConn            )
import Auth.Session      ( getToken
                         , putUltDest
                         , SessionServerPart )
import qualified View.RenderContext as RC
import qualified View.Template as T

-- |A database connection in a quantum state of uncertainty. It doesn't matter
--  whether or not it's a valid connection until we look inside.
type SchrodingerConn = Either String DBConn

-- |Make sure the request is sane (no path segments after *.css or *.js or
--  whatever); if so, serve the file with MIME type "text/css"
nullDirServe :: (ToMessage t) => t -> MIMEType -> ServerPart Response
nullDirServe template mimeT = nullDir >> ok (toResMime template mimeT)

withConn :: (Monad m) => SchrodingerConn -> (String -> m a) -> (DBConn -> m a) -> m a
withConn eitherConn failAction successAction =
    case eitherConn of
        Left err -> failAction err
        Right conn -> successAction conn

withConnErrBox :: SchrodingerConn -> (DBConn -> SessionServerPart Html) -> SessionServerPart Html
withConnErrBox eitherConn = withConn eitherConn (return . T.errBoxRawHtml)

-- |Attempt to run a SQL query given a 'DBConn', a query function from 'Query',
-- and a function to run on success. The success function uses the results of
-- the query to construct an HTML fragment. On failure, uses 'T.errBoxT'
-- to display an error on the frontend.
tryQuery :: DBConn -> (DBConn -> IO (Either String a)) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- queryF conn
    case eitherErrResults of
        Left err        -> return $ T.errBoxRawHtml err
        Right results   -> successAction results

requireLogin :: SessionServerPart Response -> SessionServerPart Response
requireLogin action = do
    maybeToken   <- getToken
    rqData       <- askRq
    let needLogin = "You must be logged in to access this page."
    case maybeToken of
        Nothing    -> putUltDest (Just $ rqUri rqData) >> (ok . toResponse . T.loginPageT $ RC.errorRenderContext needLogin)
        --TODO actually check the token
        Just token -> action

getUserInfo :: DBConn -> SessionServerPart (Either String RC.UserInfo)
getUserInfo conn = do
    maybeToken <- getToken
    case maybeToken of
        --If user has no token in their cookie, return false
        Nothing -> return $ Right RC.LoggedOut
        Just token -> do
            queryR <- liftIO $ verifySessToken token conn
            case queryR of
                Left e -> return $ Left e
                Right maybeUsername ->
                    case maybeUsername of
                        --If their token doesn't exist in the database,
                        --make the user log in.
                        Nothing -> return $ Right RC.LoggedOut
                        Just u -> return . Right $ RC.LoggedIn u

--getUsername :: DBConn -> SessionServerPart (Either String String)
--getUsername conn = do eitherInfo <- getUserInfo conn
--                      return $ eitherInfo >>= (\info -> case info of
--                                                  RC.LoggedOut -> Left "User is not logged in."
--                                                  RC.LoggedIn u -> Right u
--                                              )

isLoggedIn :: DBConn -> SessionServerPart (Either String Bool)
isLoggedIn conn = do eitherInfo <- getUserInfo conn
                     return $ eitherInfo >>= (\info -> case info of
                                                 RC.LoggedOut  -> Right False
                                                 RC.LoggedIn _ -> Right True
                                             )

getBaseContext :: DBConn -> SessionServerPart RC.RenderContext
getBaseContext conn = do
    eUserInfo <- getUserInfo conn
    case eUserInfo of
        Left e -> return $ RC.errorRenderContext e
        Right uInfo -> return $ RC.emptyRenderContext {RC.user=uInfo}

provideContext :: DBConn -> IO Html -> SessionServerPart RC.RenderContext
provideContext conn html = do
    htmlResult <- liftIO html
    baseContext <- getBaseContext conn
    return $ RC.contentOrErrors baseContext htmlResult

respondWithErr :: (RC.RenderContext -> Html) -> String ->  SessionServerPart Response
respondWithErr templateF e = ok . toResponse . templateF $ RC.emptyRenderContext {RC.errors = [e]}
