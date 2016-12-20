{-# LANGUAGE FlexibleContexts #-}

module Config where

import Data.ConfigFile
import Control.Monad.IO.Class (liftIO, MonadIO)
--import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT, MonadError, withExceptT, ExceptT)
import Control.Monad (join)

data DBAuth = DBAuth { host :: String
                     , user :: String
                     , pass :: String }

parseConfig' :: ExceptT CPError IO DBAuth
parseConfig' = do
    cp <- join $ liftIO $ readfile emptyCP "conf/sparkive.conf"
    host <- get cp "Database" "host"
    user <- get cp "Database" "user"
    pass <- get cp "Database" "pass"
    return $ DBAuth host user pass

parseConfig :: ExceptT String IO DBAuth
parseConfig = withExceptT prettyPrintErr parseConfig'

--More on this later. Bad user experience for now
prettyPrintErr :: CPError -> String
prettyPrintErr (errDat,errStr) = 
    case errDat of
        ParseError str -> "The " ++ confFile ++ " appears to be malformed. Here's the parse error: " ++ str
        NoSection str -> "The section \"" ++ str ++ "\" does not exist in the " ++ confFile ++ "."
        NoOption str -> "The option \"" ++ str ++ "\" does not exist in the " ++ confFile ++ "."
        OtherProblem str -> "There was an error processing the " ++ confFile ++ ". Here's the error: " ++ str
        _ -> error "Could not process configuration file."

    where confFile = "configuration file for this Sparkive installation (sparkive.conf)"
    
