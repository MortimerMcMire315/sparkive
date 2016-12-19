{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DBConn where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, ExceptT, runExceptT)
import qualified Config

getConn' :: ExceptT String IO Connection
getConn' = do
    confData <- liftIO Config.parseConfig
    case confData of
        Left e -> throwError $ Config.prettyPrintErr e
        Right (Config.DBAuth host user pass) -> liftIO $ do
            conn <- connect $ ConnectInfo host 5432 user pass "sparkive"
            return conn

getConn :: IO (Either String Connection)
getConn = runExceptT getConn'

unsafeExampleQuery :: Connection -> IO [[String]]
unsafeExampleQuery conn = query_ conn "SELECT attribute.attr_name,attr_values.attr_value FROM item_attrs, attr_values, attribute WHERE item_attrs.item_id=1 AND item_attrs.attr_value_id=attr_values.id AND attr_values.attr_id=attribute.id"
