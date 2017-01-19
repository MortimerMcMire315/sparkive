module View.LoginView ( login
                      , loginPost
                      ) where

-- External modules
import Control.Monad          ( msum            )
import Control.Monad.IO.Class ( liftIO          )
import Data.Maybe             ( fromMaybe       )
import Data.ByteString        ( ByteString      )
import Happstack.Server       ( ok
                              , look
                              , toResponse
                              , seeOther
                              , ServerPart
                              , Response
                              , method
                              , Method ( GET
                                       , HEAD
                                       , POST ) )
import Text.Blaze.Html        ( toHtml          )

-- Local modules
import View.Util    ( EDBConn
                    , withConn
                    , respondWithErr    )
import Auth.Session ( SessionServerPart
                    , getToken
                    , putToken
                    , putUltDest
                    , getUltDest        )
import Auth.Login   ( isCorrectPass     )
import DB.Query     ( checkUserExists   )
import DB.Types     ( DBConn            )
import Util.Random  ( getRandomToken    )
import qualified View.Template as T

emptyHtml = toHtml ""

login :: EDBConn -> SessionServerPart Response
login eitherConn = msum [ method [GET,HEAD] >> (ok . toResponse $ T.loginPageT emptyHtml)
                        , method POST >> loginPost eitherConn
                        ]

loginPost :: EDBConn -> SessionServerPart Response
loginPost eitherConn = withConn eitherConn
    --If connection fails, respond with error
    (respondWithErr T.loginPageT)
    (\conn -> do
        uname <- look "username"
        pass <- look "password"
        --Attempt to log in to the database
        loginResults <- liftIO $ doLogin uname pass conn
        case loginResults of
            --If the login failed, serve the login page with the error displayed.
            Left err  -> respondWithErr T.loginPageT err
            --Login success.
            Right sessToken -> storeSession uname sessToken conn
    )

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

storeSession :: String -> ByteString -> DBConn -> SessionServerPart Response
storeSession uname sessToken conn = do
    ultDest <- getUltDest

    --If there is an ultimate destination, set it as the next URL. otherwise,
    --go to the admin panel.
    let nextUrl = fromMaybe "/admin-panel" ultDest

    --Clear ultDest so that the user doesn't get unexpectedly redirected later
    putUltDest Nothing

    --Store the session token in the user's cookie and in the database
    storeTokenResult <- putToken uname sessToken conn
    case storeTokenResult of
        --Something truly god-awful happened if this returns Left
        Left err' -> respondWithErr T.loginPageT err'
        Right ()  -> seeOther nextUrl (toResponse "")
