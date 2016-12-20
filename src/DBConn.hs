{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DBConn where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, ExceptT, runExceptT, throwError, catchError)
import Control.Exception (catch)

import qualified Config

getConn :: IO (Either String Connection)
getConn = do
    runExceptT $ do
        (Config.DBAuth host user pass) <- Config.parseConfig
        liftIO $ putStrLn "debug1"
        conn <- liftIO . connect $ ConnectInfo host 5432 user pass "nonexistent_db"
        liftIO $ putStrLn "debug2"
        return conn

unsafeExampleQuery :: Connection -> IO [[String]]
unsafeExampleQuery conn = query_ conn "SELECT attribute.attr_name,attr_values.attr_value FROM item_attrs, attr_values, attribute WHERE item_attrs.item_id=1 AND item_attrs.attr_value_id=attr_values.id AND attr_values.attr_id=attribute.id"
