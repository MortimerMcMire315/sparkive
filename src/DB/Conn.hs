{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DB.Conn
    ( getConn
    ) where

import Control.Monad              ( join                )
import Control.Monad.Catch        ( handleIOError
                                  , throwM
                                  , catches             )
import Control.Monad.IO.Class     ( liftIO              )
import Data.Acid                  ( openLocalStateFrom  )
import Data.Default               ( def                 )
import Database.PostgreSQL.Simple ( Connection
                                  , ConnectInfo(..)
                                  , connect             )
import GHC.Word                   ( Word16              )
import Text.Read                  ( readMaybe           )

import DB.Types ( DBInfo(..)
                , PostgresAuth(..)
                , DBConn(..) )
import DB.AcidStateBackend ( Archive )
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
tryPostgresConn :: PostgresAuth -> Word16 -> IO Connection
tryPostgresConn (PostgresAuth host user pass _ dbname) safePort = do
           eitherConn <- handleIOError E.handleIOError' $
                            fmap Right . connect $ ConnectInfo host safePort user pass dbname
           case eitherConn of
               Left err   -> throwM (E.SQLConnectionException err)
               Right conn -> return conn

getPostgresConn :: PostgresAuth -> IO DBConn
getPostgresConn postgresAuth = do
    let unsafePort = port postgresAuth
    let invalidPortStr = "The port \"" ++ unsafePort ++ "\" specified in sparkive.conf has an invalid format. \
                         \Please check that it is an integer between 0 and 65535."
    let maybePort = checkPort unsafePort
    case maybePort of
        Nothing -> throwM $ E.InvalidPortException invalidPortStr
        Just p  -> fmap (PostgresConn (user postgresAuth)) (tryPostgresConn postgresAuth p)

getAcidStateConn :: FilePath -> IO DBConn
getAcidStateConn p = fmap (AcidStateConn p) (openLocalStateFrom p (def::Archive))

-- |Try to connect to a PostgreSQL database by parsing the user's config file.
--  Returns either an error string or a connection.
getConn :: IO (Either String DBConn)
getConn = do
   dbInfo <- catches (Right <$> Config.parseConfig) [ E.handleConfigParseException ]
   case dbInfo of
       Left err -> return $ Left err
       Right info ->
           case info of
               PostgresInfo dbauth ->
                   catches (Right <$> getPostgresConn dbauth)
                           [ E.handleInvalidPortException
                           , E.handleSQLConnectionException ]
               AcidStateInfo dir ->
                   catches (Right <$> getAcidStateConn dir)
                           [ E.handleErrorCall ]

--exampleQuery :: Connection -> IO [[String]]
--exampleQuery conn = liftIO $ query_ conn "SELECT attribute.attr_name,attr_values.attr_value \
--                                                \FROM item_attrs, attr_values, attribute \
--                                                \WHERE item_attrs.item_id=1 \
--                                                    \AND item_attrs.attr_value_id=attr_values.id \
--                                                    \AND attr_values.attr_id=attribute.id"
