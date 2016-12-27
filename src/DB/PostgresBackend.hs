{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.PostgresBackend where

import Database.PostgreSQL.Simple
import System.IO.Error (IOError)
import GHC.Int (Int64)
import Data.String.Utils (replace)
import Control.Monad.Catch (catches, catch, throwM)
import Data.ByteString (ByteString)

import qualified Exception.Handler as E
import Exception.Util (handles)

-- |Attempt to select from the "attribute" table in the Sparkive database.
--  If the selection fails, we say that the database does not exist or has
--  been partially deleted.
checkDBExists :: Connection -> IO Bool
checkDBExists conn = do
    eitherErrResults <- handles [E.handleSQLError] $ fmap Right (query_ conn "SELECT * FROM attribute" :: IO [(Int, String)])
    case eitherErrResults of
        Left  _ -> return False
        Right _ -> return True -- I'm frustrated that there's not a specific exception type or constructor
                               -- for table nonexistence. TODO conjure a better way?

-- TODO Incomplete and unsafe
-- |Parse the "db/create.sql" file, and use it to create the necessary tables
--  for a Sparkive installation.
createDB :: String -> Connection -> IO ()
createDB user conn = do
    let filepath = "db/create.sql"
    let fileErrStr = "Could not open file \"" ++ filepath ++
                    "\". Please check that the file exists and is readable in\
                    \ your Sparkive installation."
    (f :: String) <- catch (readFile filepath) (\(x :: IOError) ->
                          throwM $ E.ReadFileException fileErrStr
                                               )
    putStrLn "still here."

    let (q :: Query) = read . show $ replace "%user%" user f
    execute_ conn q
    return ()

insertUser :: String -> ByteString -> Connection -> IO ()
insertUser username pass conn = do
    execute conn "INSERT INTO sparkive_user (username, pass) VALUES (?,?)" (username, pass)
    return ()

-- |Throws GHC.Exception.ErrorCall and all SQL exceptions.
--  Handle with handleErrorCall and sqlErrorHandlers.
getPassHash :: String -> Connection -> IO [ByteString]
getPassHash username conn = do
    results <- query conn "SELECT pass FROM sparkive_user WHERE username = ?" (Only username)
    return $ map fromOnly results
