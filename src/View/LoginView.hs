module View.LoginView ( login
                      , loginPost
                      ) where

import Control.Monad          ( msum            )
import Control.Monad.IO.Class ( liftIO          )
import Happstack.Server       ( ok
                              , look
                              , toResponse
                              , ServerPart
                              , Response
                              , method
                              , Method ( GET
                                       , HEAD
                                       , POST ) )

import View.Util              ( EDBConn         )
import qualified View.Template as T

login :: EDBConn -> ServerPart Response
login eitherConn = msum [ method [GET,HEAD] >> (ok . toResponse $ T.loginPageT Nothing)
                        , method POST >> loginPost eitherConn
                        ]

loginPost :: EDBConn -> ServerPart Response
loginPost eitherConn = do
    uname <- look "username"
    pass <- look "password"
    ok . toResponse $
       T.loginPageT (Just $ T.errBoxT ("Username: " ++ uname ++ " ; Pass: " ++ pass))
--
--doLogin :: EDBConn -> Either
--doLogin eitherConn = do
