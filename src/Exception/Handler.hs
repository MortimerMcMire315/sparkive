{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Exception.Handler 
    ( ConfigParseException(..)
    , InvalidPortException(..)
    , ReadFileException(..)
    , SQLConnectionException(..)
    , handleConfigParseException
    , handleInvalidPortException
    , handleReadFileException
    , handleSQLConnectionException
    , handleIOError'
    , handleSQLError
    , handleSQLResultError
    , handleSQLQueryError
    , handleSQLFormatError
    , sqlErrorHandlers
    ) where

import Data.Typeable.Internal (Typeable)
import Control.Exception (Exception)
import Control.Monad.Catch (Handler (..), MonadCatch, MonadThrow)
import Database.PostgreSQL.Simple (SqlError(..), ResultError(..), QueryError(..), FormatError(..))

import Exception.TH

htmlNewlines :: String -> String
htmlNewlines str = htmlNewlines' str ""

htmlNewlines' :: String -> String -> String
htmlNewlines' "" scanned = reverse scanned
htmlNewlines' (x:xs) scanned = if x == '\n' 
                                then htmlNewlines' xs $ ('>':'r':'b':'<':scanned)
                                else htmlNewlines' xs $ (x:scanned)

$(buildErrorHandler "ConfigParseException")
$(buildErrorHandler "InvalidPortException")
$(buildErrorHandler "ReadFileException")

-- Unfortunately, I believe I have to use an IO error catch-all when dealing
-- with errors from invalid attempts to use the "connect" function in
-- 'Database.PostgreSQL.Simple'. The error comes from libPQ and uses throwIO.
handleIOError' :: IOError -> IO (Either String a)
handleIOError' e = do
    putStrLn $ show e
    return . Left . htmlNewlines $ show e

{----------------}
{-- SQL ERRORS --}
{----------------}

$(buildErrorHandler "SQLConnectionException")

-- |See Database.PostgreSQL.Simple.SqlError
handleSQLError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSQLError = Handler (\(SqlError state status msg detail hint) -> do
                            return . Left $ prefix ++ (show msg))
    where prefix = "The PostgreSQL backend returned an error: "

-- |See 'Database.PostgreSQL.Simple.ResultError'
handleSQLResultError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSQLResultError = Handler 
    (\err -> case err of 
        (Incompatible sqlType sqlTableOid sqlField haskellType msg) ->
            return . Left $ prefix ++ "The SQL type " ++ sqlType ++ 
                            " is not compatible with the Haskell type "
                            ++ haskellType ++ ". <br><br>Additional info: <br><br>" 
                            ++ msg
        (UnexpectedNull sqlType sqlTableOid sqlField haskellType msg) ->
            return . Left $ prefix ++ "The SQL type " ++ sqlType ++ 
                            " returned NULL, while the Haskell type "
                            ++ haskellType ++ " does not permit null values."
                            ++ "<br><br>Additional info: <br><br>" ++ msg
        (ConversionFailed sqlType sqlTableOid sqlField haskellType msg) ->
            return . Left $ prefix ++ "Conversion failed: A SQL value of type " ++ sqlType ++ 
                            " could not be represented as a value of Haskell type "
                            ++ haskellType ++ ", or else a low-level error occured."
                            ++ "<br><br>Additional info: <br><br>" ++ msg
    )
    where prefix = "SQL Result error occurred: "

-- |See Database.PostgreSQL.Simple.QueryError
handleSQLQueryError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSQLQueryError = Handler
    (\(QueryError _ _ ) -> return . Left $ "QueryError: the 'query' function was used to perform an INSERT-like " ++
                        "operation, or 'execute' was used to perform a SELECT-like operation."
    )

-- |See Database.PostgreSQL.Simple.FormatError
handleSQLFormatError :: (MonadCatch m, MonadThrow m) => Handler m (Either String a)
handleSQLFormatError = Handler
    (\(FormatError _ _ _) -> return . Left $ "FormatError: The SQL query could not be formatted correctly.")

-- Convenient to include these all at once when running queries.
sqlErrorHandlers :: (MonadCatch m, MonadThrow m) => [Handler m (Either String a)]
sqlErrorHandlers = [handleSQLError, handleSQLResultError, handleSQLQueryError, handleSQLFormatError]
