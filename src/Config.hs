{-# LANGUAGE FlexibleContexts #-}

module Config ( parseConfig
              ) where

import Data.ConfigFile        ( readfile
                              , get
                              , emptyCP
                              , CPError(..)
                              , CPErrorData(..) )
import Control.Monad.IO.Class ( liftIO          )
import Control.Monad.Except   ( runExceptT      )
import Control.Monad.Catch    ( throwM
                              , MonadThrow      )
import Control.Monad          ( join            )

import qualified Exception.Handler as E
import DB.Types ( DBInfo(..)
                , PostgresAuth(..)
                , DBConn(..))

-- |Attempt to parse the configuration file located in conf/sparkive.conf.
parseConfig' :: IO (Either CPError DBInfo)
parseConfig' = runExceptT $ do
    cp <- join . liftIO $ readfile emptyCP "conf/sparkive.conf"
    backend <- get cp "Database" "backend"
    if backend == "postgres"
    then do
        host   <- get cp "Postgres" "host"
        user   <- get cp "Postgres" "user"
        pass   <- get cp "Postgres" "pass"
        port   <- get cp "Postgres" "port"
        dbname <- get cp "Postgres" "db_name"
        return . PostgresInfo $ PostgresAuth host user pass port dbname
    else if backend == "acid-state"
         then do
            acidPath <- get cp "Acid-State" "dir"
            return $ AcidStateInfo acidPath
         else liftIO . throwM $ E.ConfigParseException wrongDBType
  where wrongDBType = "Database type in sparkive.conf must be either \"acid-state\" or \"postgres\""


-- |Parse the configuration file as in 'parseConfig\'', but simply throw an exception
--  if an error occurs.
parseConfig :: IO DBInfo
parseConfig = do
    pc <- parseConfig'
    case pc of
        Left cperr -> throwM (E.ConfigParseException $ prettyPrintErr cperr)
        Right x -> return x


-- |Return a string describing the given 'CPError' in a more user-friendly way.
prettyPrintErr :: CPError -> String
prettyPrintErr (errDat,errStr) =
    case errDat of
        ParseError str -> "The " ++ confFile ++ " appears to be malformed. Here's the parse error: " ++ str
        NoSection str -> "The section \"" ++ str ++ "\" does not exist in the " ++ confFile ++ "."
        NoOption str -> "The option \"" ++ str ++ "\" does not exist in the " ++ confFile ++ "."
        OtherProblem str -> "There was an error processing the " ++ confFile ++ ". Here's the error: " ++ str
        _ -> error "Could not process configuration file."

    where confFile = "configuration file for this Sparkive installation (sparkive.conf)"
