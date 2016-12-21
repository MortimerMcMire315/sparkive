{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Exception.Handler where

import Data.Typeable.Internal (Typeable)
import Control.Exception (Exception)
import Control.Monad.Catch (Handler (..), MonadCatch, MonadThrow)
import Database.PostgreSQL.Simple (SqlError(..))

import Exception.TH


htmlNewlines :: String -> String
htmlNewlines str = htmlNewlines' str ""

htmlNewlines' :: String -> String -> String
htmlNewlines' "" scanned = reverse scanned
htmlNewlines' (x:xs) scanned = if x == '\n' 
                                then htmlNewlines' xs $ ('>':'r':'b':'<':scanned)
                                else htmlNewlines' xs $ (x:scanned)

$(buildErrorHandler "ConfigParseException")
$(buildErrorHandler "SQLConnectionException")
$(buildErrorHandler "InvalidPortException")

-- Unfortunately, I believe I have to use an IO error catch-all when dealing
-- with errors from invalid attempts to use the "connect" function in
-- Database.PostgreSQL.Simple. The error comes from libPQ and uses throwIO.
handleIOError' :: IOError -> IO (Either String a)
handleIOError' e = do
    putStrLn $ show e
    return . Left . htmlNewlines $ show e

handleSQLError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSQLError = Handler (\(SqlError state status msg detail hint) -> do
                            let prefix = "SQL error occurred: "
                            return . Left $ prefix ++ (show msg))
