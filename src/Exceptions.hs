module Exceptions where

import Data.Typeable.Internal (Typeable)
import Control.Exception (Exception)
import Control.Monad.Catch (Handler (..), MonadCatch, MonadThrow)
import Database.PostgreSQL.Simple (SqlError(..))


escapeNewlines :: String -> String
escapeNewlines str = escapeNewlines' str ""

escapeNewlines' :: String -> String -> String
escapeNewlines' "" scanned = reverse scanned
escapeNewlines' (x:xs) scanned = if x == '\n' 
                                then escapeNewlines' xs $ ('>':'r':'b':'<':scanned)
                                else escapeNewlines' xs $ (x:scanned)

--TODO Template Haskell this file, because I will handle 99% of homemade exceptions the same way???
data ConfigParseException = ConfigParseException String deriving (Typeable, Show)
instance Exception ConfigParseException
handleConfigParseException :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleConfigParseException = Handler (\(ConfigParseException str) -> return $ Left str)

handleIOError :: IOError -> IO (Either String a)
handleIOError e = do
    putStrLn $ show e
    return . Left . escapeNewlines $ show e

data SqlConnectionException = SqlConnectionException String deriving (Typeable, Show)
instance Exception SqlConnectionException
handleSqlConnectionException :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSqlConnectionException = Handler (\(SqlConnectionException str) -> return $ Left str)

handleSqlError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSqlError = Handler (\(SqlError state status msg detail hint) -> return . Left $ show msg)

--handleSqlError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
--handleSqlError = Handler (\(SqlError state execStatus errorMsg errorDetail errorHint) ->
--                             return . Left $ show errorMsg)
