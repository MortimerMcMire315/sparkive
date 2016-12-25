{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DB.Conn
    ( getConn
    , exampleQuery
    ) where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (handleIOError, throwM)
import GHC.Word (Word16)
import Text.Read (readMaybe)

import qualified Exception.Handler as E
import qualified Config

-- |Given a string, check if it represents a valid port number.
checkPort :: String -> Maybe Word16
checkPort unsafePort = (readMaybe unsafePort :: Maybe Int) >>=
                           checkPortRange >>
                               (readMaybe unsafePort :: Maybe Word16)
    where checkPortRange x = if x < 0 || x > 65535 then Nothing else Just ()

-- |Try to connect to a PostgreSQL database using the given credentials. If this fails,
--  catch the IOError and re-throw it as a 'E.SQLConnectionException'.
tryConn :: Config.DBAuth -> Word16 -> IO Connection
tryConn (Config.DBAuth host user pass _ dbname) safePort = do
           eitherConn <- handleIOError E.handleIOError' $
                            fmap Right . connect $ ConnectInfo host safePort user pass dbname
           case eitherConn of
               Left err   -> throwM (E.SQLConnectionException err)
               Right conn -> return conn

-- |Try to connect to a PostgreSQL database by parsing the user's config file.
--  Throws 'E.InvalidPortException' if the port listed in the configuration is invalid
--  and 'E.SQLConnectionException' if there is another error in connecting to the DB.
getConn :: IO Connection
getConn = do
    dbAuth <- Config.parseConfig
    let unsafePort = Config.port dbAuth
    let invalidPortStr = "The port \"" ++ unsafePort ++ "\" specified in sparkive.conf has an invalid format. \
                         \Please check that it is an integer between 0 and 65535."
    let maybePort = checkPort unsafePort
    case maybePort of
        Nothing   -> throwM $ E.InvalidPortException invalidPortStr
        Just p -> tryConn dbAuth p

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
