{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.PostgresBackend where

import Control.Monad              ( void       )
import Control.Monad.Catch        ( catches
                                  , catch
                                  , throwM     )
import Data.ByteString            ( ByteString )
import Data.String.Utils          ( replace    )
import Database.PostgreSQL.Simple ( query_
                                  , query
                                  , execute_
                                  , execute
                                  , Binary(..)
                                  , Connection
                                  , Query
                                  , Only(..)   )
import GHC.Int                    ( Int64      )
import System.IO.Error            ( IOError    )

import qualified Exception.Handler as E
import Exception.Util ( handles, rethrowIO )

-- |Alias for "const ()" to use tryQuery without transforming results
rawResults = const ()

-- |Attempt to select from the "attribute" table in the Sparkive database.
--  If the selection fails, we say that the database does not exist or has
--  been partially deleted.
checkDBExists :: Connection -> IO (Either String Bool)
checkDBExists conn = do
    let q = fmap Right (query_ conn "SELECT * FROM attribute" :: IO [(Int, String)])

    --Unfortunately, I'm assuming that the query is valid in all cases except those where the
    --table doesn't exist.
    eitherErrResults <- catches q [E.handleSQLError]

    return $ case eitherErrResults of
        Left _ -> Right False
        Right _ -> Right True

-- |Given a function to process query results and the query itself,
--  return the processed results of the query. The function is given to
--  avoid incessant fmapping.
tryQuery :: (a -> b) -> IO a -> IO (Either String b)
tryQuery f q = catches (fmap (Right . f) q) $ E.handleErrorCall : E.sqlErrorHandlers

-- |Parse the "db/create.sql" file, and use it to create the necessary tables
--  for a Sparkive installation.
createDB :: String -> Connection -> IO (Either String ())
createDB user conn = do
    let filepath = "db/create.sql"
    let fileErrStr = "Could not open file \"" ++ filepath ++
                    "\". Please check that the file exists and is readable in\
                    \ your Sparkive installation."
    f <- rethrowIO (readFile filepath) E.ReadFileException E.handleReadFileException fileErrStr
--    f <- catches (readFile filepath)
--    (f :: String) <- catches ( catch (readFile filepath) (\(x :: IOError) ->
--                                  throwM $ E.ReadFileException fileErrStr
--                                                         )
--                             ) [E.handleReadFileException]
    case f of
        Left err  -> return $ Left err
        Right fileStr -> do
            let (q :: Query) = read . show $ replace "%user%" user fileStr
            tryQuery rawResults $ execute_ conn q

--TODO!!! Check if user already exists. Or did we do this elsewhere? Where should I put it????
insertUser :: String -> ByteString -> ByteString -> Connection -> IO (Either String ())
insertUser username pass salt conn = tryQuery rawResults $
        execute conn "INSERT INTO sparkive_user (username, pass, salt) VALUES (?,?,?)" (username, pass, salt)

getUserAttr :: IO [Only ByteString]-> IO (Either String ByteString)
getUserAttr q = do
    res <- tryQuery (map fromOnly) q
    return $ res >>=
      (\bss ->
        if length bss /= 1
        then Left errStr
        else Right $ head bss
      )
 where errStr = "User has more than one row in the database. Aborting."

getSalt :: String -> Connection -> IO (Either String ByteString)
getSalt username conn = getUserAttr
    (query conn "SELECT salt FROM sparkive_user WHERE username = ?" (Only username))

getPassHash :: String -> Connection -> IO (Either String ByteString)
getPassHash username conn = getUserAttr
    (query conn "SELECT pass FROM sparkive_user WHERE username = ?" (Only username))

checkUserExists :: String -> Connection -> IO (Either String Bool)
checkUserExists username conn = do
    res <- tryQuery id
                    (query conn "SELECT * FROM sparkive_user WHERE username = ?"
                                (Only username)
                                :: IO [(Int, ByteString, ByteString, ByteString)]
                    )
    return $ res >>=
      (\bss ->
        if length bss > 1
        then Left $ "Error: More than one user with username \"" ++ username ++ "\" found in database."
        else Right (not $ null bss)
      )

insertSessToken :: String -> ByteString -> Connection -> IO (Either String ())
insertSessToken username token conn = tryQuery rawResults $
    execute conn "INSERT INTO sess_token (username, token) VALUES (?,?)" (username, Binary token)
