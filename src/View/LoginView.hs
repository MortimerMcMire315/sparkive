module View.LoginView ( login
                      , loginPost
                      ) where

import Control.Monad          ( msum            )
import Control.Monad.IO.Class ( liftIO          )
import Data.ByteString        ( ByteString      )
import Happstack.Server       ( ok
                              , look
                              , toResponse
                              , ServerPart
                              , Response
                              , method
                              , Method ( GET
                                       , HEAD
                                       , POST ) )

import View.Util    ( EDBConn, withConn )
import Auth.Session ( SessionServerPart
                    , SessionData
                    , token             )
import Auth.Login   ( isCorrectPass     )
import DB.Query     ( checkUserExists   )
import DB.Types     ( DBConn            )
import Util.Random  ( getRandomToken    )
import qualified View.Template as T

login :: EDBConn -> SessionServerPart Response
login eitherConn = msum [ method [GET,HEAD] >> (ok . toResponse $ T.loginPageT Nothing)
                        , method POST >> loginPost eitherConn
                        ]

loginPost :: EDBConn -> SessionServerPart Response
loginPost eitherConn = do
    uname <- look "username"
    pass <- look "password"
    loginResults <- doLogin eitherConn uname pass 
    case loginResults of
        Left err  -> ok . toResponse $ T.loginPageT (Just $ T.errBoxT err)
        Right str -> ok . toResponse $ T.loginPageT (Just $ T.errBoxT (show str))

doLogin :: EDBConn -> String -> String -> SessionServerPart (Either String ByteString)
doLogin eitherConn uname pass = liftIO $
        withConn eitherConn
                 (return . Left)
                 (\conn -> tryLogin uname pass conn)

tryLogin :: String -> String -> DBConn ->  IO (Either String ByteString)
tryLogin uname pass conn = do
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
                         then Right <$> getRandomToken
                         else return . Left $ "Incorrect password."
