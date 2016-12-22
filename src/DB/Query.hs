{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.Query (createDB, checkDBExists) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Control.Monad.Catch (catches, catch, throwM)
import Data.String.Utils (replace)
import System.IO.Error (IOError)
import GHC.Int (Int64)

import qualified Exception.Handler as E
import Exception.Util (handles)

checkDBExists :: Connection -> IO Bool
checkDBExists conn = do
    eitherErrResults <- handles [E.handleSQLError] $ fmap Right (query_ conn "SELECT * FROM attribute" :: IO [[String]])
    case eitherErrResults of
        Left  _ -> return False
        Right _ -> return True -- I'm frustrated that there's not a specific exception type or constructor
                               -- for table nonexistence. TODO conjure a better way?

-- TODO Incomplete and unsafe
createDB :: Connection -> String -> IO Int64
createDB conn user = do
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
