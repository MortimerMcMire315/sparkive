{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DBConn where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (handleIOError, throwM)

import qualified Exceptions as E
import qualified Config

getConn :: IO Connection
getConn = do
    (Config.DBAuth host user pass) <- Config.parseConfig
    putStrLn $ show host
    putStrLn "debug1"
    eitherConn <- handleIOError E.handleIOError $ 
                    fmap Right . connect $ ConnectInfo host 5433 user pass "sparkive"
    case eitherConn of
        Left err -> throwM (E.SqlConnectionException err)
        Right conn -> do putStrLn "debug2"
                         return conn 

exampleQuery :: Connection -> IO [[String]]
exampleQuery conn = liftIO $ query_ conn "SELECT attribute.attr_name,attr_values.attr_value \
                                                \FROM item_attrs, attr_values, attribute \
                                                \WHERE item_attrs.item_id=1 \
                                                    \AND item_attrs.attr_value_id=attr_values.id \
                                                    \AND attr_values.attr_id=attribute.id"

--performQuery :: ExceptT String IO Connection -> (Connection -> ExceptT String IO a) -> IO (Either String a)
--performQuery conn qf = runExceptT $ do
--    c <- conn
--    qf c
