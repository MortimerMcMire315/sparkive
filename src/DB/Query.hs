
module DB.Query
    ( createDB
    , checkDBExists
    , insertUser
    , getPassHash
    ) where

import Data.ByteString (ByteString)
import Data.Acid (query, update)
import Control.Monad (void)

import qualified DB.PostgresBackend as PG
import qualified DB.AcidStateBackend as AS
import DB.Types

data DBReturn

checkDBExists :: DBConn -> IO Bool
checkDBExists (PostgresConn _ c)  = PG.checkDBExists c
checkDBExists (AcidStateConn _ a) = return True

createDB :: String -> DBConn -> IO ()
createDB u c =
    case c of
        (PostgresConn _ pgc) -> PG.createDB u pgc
        (AcidStateConn _ a)  -> return ()

insertUser :: String -> ByteString -> DBConn -> IO ()
insertUser username passHash (PostgresConn _ c)  = PG.insertUser username passHash c
insertUser username passHash (AcidStateConn _ a) = update a (AS.InsertUser username passHash) >>= (\_ -> return ())

getPassHash :: String -> DBConn -> IO [ByteString]
getPassHash username (PostgresConn _ c)  = PG.getPassHash username c
getPassHash username (AcidStateConn _ a) = query a (AS.GetPassHash username)
