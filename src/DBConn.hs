{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DBConn where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (handleIOError, throwM)
import GHC.Word (Word16)
import Text.Read (readMaybe)

import qualified Exception.Handler as E
import qualified Config

getConn :: IO Connection
getConn = do
    (Config.DBAuth host user pass unsafePort dbname) <- Config.parseConfig
    let invalidPortStr = "The port \"" ++ unsafePort ++ "\" specified in sparkive.conf has an invalid format. \
                         \Please check that it is an integer of 5 or fewer digits."
    maybePort <- return (readMaybe unsafePort :: Maybe Word16)
    case maybePort of
        Nothing   -> throwM (E.InvalidPortException invalidPortStr)
        Just port -> do
            eitherConn <- handleIOError E.handleIOError' $ 
                             fmap Right . connect $ ConnectInfo host port user pass dbname
            case eitherConn of
                Left err   -> throwM (E.SQLConnectionException err)
                Right conn -> return conn 

exampleQuery :: Connection -> IO [[String]]
exampleQuery conn = liftIO $ query_ conn "SEiLECT attribute.attr_name,attr_values.attr_value \
                                                \FROM item_attrs, attr_values, attribute \
                                                \WHERE item_attrs.item_id=1 \
                                                    \AND item_attrs.attr_value_id=attr_values.id \
                                                    \AND attr_values.attr_id=attribute.id"

--performQuery :: ExceptT String IO Connection -> (Connection -> ExceptT String IO a) -> IO (Either String a)
--performQuery conn qf = runExceptT $ do
--    c <- conn
--    qf c
