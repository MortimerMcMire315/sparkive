
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

checkDBExists :: DBConn -> IO (Either String Bool)
checkDBExists (PostgresConn _ c)  = PG.checkDBExists c
checkDBExists (AcidStateConn _ a) = return $ Right True --Because like, the tables are /in the code/, man.

createDB :: String -> DBConn -> IO (Either String ())
createDB u (PostgresConn _ pgc) = PG.createDB u pgc
createDB u (AcidStateConn _ a) = return $ Right () --See above

insertUser :: String -> ByteString -> DBConn -> IO (Either String ())
insertUser username passHash (PostgresConn _ c)  = PG.insertUser username passHash c
insertUser username passHash (AcidStateConn _ a) = AS.insertUserQ username passHash a

getPassHash :: String -> DBConn -> IO (Either String ByteString)
getPassHash username (PostgresConn _ c)  = PG.getPassHash username c
getPassHash username (AcidStateConn _ a) = AS.getPassHashQ username a

