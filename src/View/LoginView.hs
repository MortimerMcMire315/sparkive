module View.LoginView ( adminPanel
                      , login
                      , loginPost
                      , doLogin
                      ) where

-- External modules
import Control.Monad          ( msum          )
import Control.Monad.IO.Class ( liftIO        )
import Data.Maybe             ( fromMaybe     )
import Data.ByteString        ( ByteString    )
import Happstack.Server       ( Method ( GET
                                       , HEAD
                                       , POST )
                              , Response
                              , ServerPart
                              , localRq
                              , look
                              , method
                              , ok
                              , rqUri
                              , seeOther
                              , toResponse    )
import Text.Blaze.Html        ( toHtml        )

-- Local modules
import View.Util    ( getBaseContext
                    , isLoggedIn
                    , requireLogin
                    , respondWithErr    )
import Auth.Session ( SessionServerPart
                    , getToken
                    , getUltDest
                    , putToken
                    , putUltDest        )
import Auth.Login   ( isCorrectPass     )
import DB.Query     ( checkUserExists   )
import DB.Types     ( DBConn            )
import Util.Random  ( getRandomToken    )
import qualified View.Template as T
import qualified View.RenderContext as RC

adminPanel :: DBConn -> SessionServerPart Response
-- Manually set the URI to /admin-panel in case the user requested /login. This
-- ensures that the ult-dest is set to /admin-panel.
adminPanel conn = htmlResult >>= (requireLogin conn . ok . toResponse)
    where htmlResult = T.adminPanelT <$> getBaseContext conn

-- |Routing for /login:
--  GET: use the 'adminPanel' view because it requires a login, then serves the
--  login page.
--  POST: Validate credentials using loginPost
login :: DBConn -> SessionServerPart Response
login conn = msum [ method [GET,HEAD] >> loginGet conn
                   , method POST       >> loginPost conn
                   ]

-- |Check if the user is already logged in. If so, redirect to /admin-panel.
--  If not, serve the login page.
loginGet :: DBConn -> SessionServerPart Response
loginGet conn = do
    eLoggedIn <- isLoggedIn conn
    case eLoggedIn of
        Left e -> respondWithErr T.loginPageT e
        Right loggedIn ->
            if loggedIn
            then seeOther "/admin-panel" (toResponse "Redirecting to /admin-panel...")
            --TODO not empty context
            else ok . toResponse $ T.loginPageT RC.emptyRenderContext

loginPost :: DBConn -> SessionServerPart Response
loginPost conn = do
    uname <- look "username"
    pass <- look "password"
    --Attempt to log in to the database
    loginResults <- liftIO $ doLogin uname pass conn
    case loginResults of
        --If the login failed, serve the login page with the error displayed.
        Left err -> respondWithErr T.loginPageT err
        --Login success.
        Right sessToken -> do
            --Store the session token in the user's cookie and in the database
            storeResult <- putToken uname sessToken conn
            serveLoginResponse uname storeResult conn

doLogin :: String -> String -> DBConn ->  IO (Either String ByteString)
doLogin uname pass conn = do
    eitherUserExists <- checkUserExists uname conn
    case eitherUserExists of
        Left err -> return $ Left err
        Right exists -> if exists
                        then passCheck uname pass conn
                        else return . Left $ "Username " ++ uname ++ " not found in database."

passCheck :: String -> String -> DBConn -> IO (Either String ByteString)
passCheck uname pass conn = do
    eCorrectPass <- isCorrectPass uname pass conn
    case eCorrectPass of
        Left err -> return $ Left err
        Right correct -> if correct
                         --TODO store in database
                         then Right <$> getRandomToken
                         else return $ Left "Incorrect password."

serveLoginResponse :: String -> Either String () -> DBConn -> SessionServerPart Response
serveLoginResponse uname storeResult conn =
    case storeResult of
        --Something truly god-awful happened if this is Left. Probably Y2K
        Left err' -> respondWithErr T.loginPageT err'
        Right ()  -> do
            ultDest <- getUltDest

            --If there is an ultimate destination, set it as the next URL. otherwise,
            --go to the admin panel.
            let nextUrl = fromMaybe "/admin-panel" ultDest

            --Clear ultDest so that the user doesn't get unexpectedly redirected later
            putUltDest Nothing
            seeOther nextUrl (toResponse "Redirecting to ultimate destination...")
