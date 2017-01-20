module DB.Query
    ( createDB
    , checkDBExists
    , insertUser
    , getPassHash
    , getSalt
    , checkUserExists
    , verifySessToken
    , insertSessToken
    ) where

import Data.ByteString            ( ByteString  )
import Data.Acid                  ( AcidState   )
import Database.PostgreSQL.Simple ( Connection  )

import DB.Types            ( DBConn(..) )
import DB.AcidStateBackend ( Archive    )
import qualified DB.PostgresBackend as PG
import qualified DB.AcidStateBackend as AS

doQuery :: (Connection -> IO a) -> (AcidState Archive -> IO a) -> DBConn -> IO a
doQuery pgQuery acidQuery conn =
    case conn of (PostgresConn _ c) -> pgQuery c
                 (AcidStateConn _ a) -> acidQuery a

checkDBExists :: DBConn -> IO (Either String Bool)
checkDBExists (PostgresConn _ c)  = PG.checkDBExists c
checkDBExists (AcidStateConn _ a) = return $ Right True --Because like, the tables are /in the code/, man.

createDB :: String -> DBConn -> IO (Either String ())
createDB u (PostgresConn _ pgc) = PG.createDB u pgc
createDB u (AcidStateConn _ a)  = return $ Right () --See above

insertUser :: String -> ByteString -> ByteString -> DBConn -> IO (Either String ())
insertUser username passHash salt = doQuery (PG.insertUser username passHash salt)
                                            (AS.insertUserQ username passHash salt)

getPassHash :: String -> DBConn -> IO (Either String ByteString)
getPassHash username = doQuery (PG.getPassHash username)
                               (AS.getPassHashQ username)

getSalt :: String -> DBConn -> IO (Either String ByteString)
getSalt username = doQuery (PG.getSalt username)
                           (AS.getSaltQ username)

checkUserExists :: String -> DBConn -> IO (Either String Bool)
checkUserExists username = doQuery (PG.checkUserExists username)
                                   (AS.checkUserExistsQ username)

-- Returns either an error string or maybe a username. If the token is not found,
-- returns (Right Nothing); if the token is found, returns (Left username)
verifySessToken :: ByteString -> DBConn -> IO (Either String (Maybe String))
verifySessToken token = doQuery (PG.verifySessToken token)
                                (AS.verifySessTokenQ token)

insertSessToken :: String -> ByteString -> DBConn -> IO (Either String ())
insertSessToken username token = doQuery (PG.insertSessToken username token)
                                         (AS.insertSessTokenQ username token)
