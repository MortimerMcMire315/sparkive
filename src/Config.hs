{-# LANGUAGE FlexibleContexts #-}

module Config where

import Data.ConfigFile
import Control.Monad.IO.Class (liftIO)
--import Control.Monad.Throw (runExceptT, MonadError, withExceptT, ExceptT)
import Control.Monad.Except (runExceptT)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad (join)

import qualified Exception.Handler as E


data DBAuth = DBAuth { host   :: String
                     , user   :: String
                     , pass   :: String 
                     , port   :: String
                     , dbname :: String} deriving Show

parseConfig' :: IO (Either CPError DBAuth)
parseConfig' = runExceptT $ do
    cp <- join $ liftIO $ readfile emptyCP "conf/sparkive.conf"
    host   <- get cp "Database" "host"
    user   <- get cp "Database" "user"
    pass   <- get cp "Database" "pass"
    port   <- get cp "Database" "port"
    dbname <- get cp "Database" "db_name"
    return $ DBAuth host user pass port dbname

parseConfig :: IO DBAuth
parseConfig = do
    pc <- parseConfig'
    case pc of
        Left cperr -> throwM (E.ConfigParseException $ prettyPrintErr cperr)
        Right x -> return x

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
    
