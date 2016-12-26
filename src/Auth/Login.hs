module Auth.Login where

import Crypto.BCrypt
import Data.ByteString.Char8 (ByteString,pack)
import qualified DB.Query as Query
import Database.PostgreSQL.Simple (Connection)
import Exception.Handler (sqlErrorHandlers, handleErrorCall)
import Exception.Util (handles)
import GHC.Int (Int64)

hashPass :: String -> IO (Maybe ByteString)
hashPass p = hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack p)

--TODO add password salts before anyone uses this over a network ¯\_(ツ)_/¯
--Also, name variables better things. Like there are only so many combinations of "hash" and "pass"
storeUser :: String -> String -> Connection -> IO (Either String Int64)
storeUser username pass conn = do
    hashed <- hashPass pass
    case hashed of
        Nothing -> return $ Left "Password could not be hashed."
        --TODO maybe move tryQuery to Query so that I don't have to do this.
        Just hashedPass -> handles sqlErrorHandlers (Right <$> Query.insertUser username hashedPass conn)

--Use Bcrypte to determine if the password for the given username is correct.
isCorrectPass :: String -> String -> Connection -> IO (Either String Bool)
isCorrectPass username pass conn = do
    hash <- handles (sqlErrorHandlers ++ [handleErrorCall]) (Right <$> Query.getPassHash username conn)
    if null hash
    then return $ Left "No results found" --TODO like, this should probably happen elsewhere
    else return $ hash >>= (\h -> Right $ validatePassword (head h) (pack pass))
