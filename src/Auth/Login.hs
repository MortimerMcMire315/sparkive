module Auth.Login where

import Crypto.BCrypt
import Data.ByteString.Char8 (ByteString,pack)
import Database.PostgreSQL.Simple (Connection)

import qualified DB.Query as Query
import DB.Types
import Exception.Handler (sqlErrorHandlers, handleErrorCall)
import Exception.Util (handles)

hashPass :: String -> IO (Maybe ByteString)
hashPass p = hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack p)

--TODO add password salts before anyone uses this over a network ¯\_(ツ)_/¯
--Also, name variables better things. Like there are only so many combinations of "hash" and "pass"
storeUser :: String -> String -> DBConn -> IO (Either String ())
storeUser username pass conn = do
    hashed <- hashPass pass
    case hashed of
        Nothing -> return $ Left "Password could not be hashed."
        --TODO maybe move tryQuery to Query so that I don't have to do this.
        Just hashedPass -> Query.insertUser username hashedPass conn

--Use Bcrypte to determine if the password for the given username is correct.
isCorrectPass :: String -> String -> DBConn -> IO (Either String Bool)
isCorrectPass username pass conn = do
    hash <- Query.getPassHash username conn
    return $ hash >>= (\h -> Right $ validatePassword h (pack pass))
