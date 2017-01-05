module Auth.Login where

import Crypto.BCrypt               ( hashPasswordUsingPolicy
                                   , genSaltUsingPolicy
                                   , fastBcryptHashingPolicy
                                   , validatePassword        )
import Data.ByteString.Char8       ( ByteString
                                   , append
                                   , pack                    )
import Database.PostgreSQL.Simple  ( Connection              )

import DB.Types                    ( DBConn                  )
import Exception.Handler           ( sqlErrorHandlers
                                   , handleErrorCall         )
import Exception.Util              ( handles                 )

import qualified DB.Query as Query

hashPass :: ByteString -> IO (Maybe ByteString)
hashPass = hashPasswordUsingPolicy fastBcryptHashingPolicy

--Also, name variables better things. Like there are only so many combinations of "hash" and "pass"
storeUser :: String -> String -> DBConn -> IO (Either String ())
storeUser username pass conn = do
    maybeSalt <- genSaltUsingPolicy fastBcryptHashingPolicy
    case maybeSalt of 
        Nothing -> return $ Left errStr
        Just salt -> do
            hashed <- hashPass $ append (pack pass) salt
            case hashed of
                Nothing -> return $ Left errStr
                Just hashedPass -> Query.insertUser username hashedPass salt conn 
  where errStr = "Password could not be hashed."

--Use Bcrypt to determine if the password for the given username is correct.
isCorrectPass :: String -> String -> DBConn -> IO (Either String Bool)
isCorrectPass username pass conn = do
    eitherHash <- Query.getPassHash username conn
    eitherSalt <- Query.getSalt username conn
    return $ do --in the Either monad
        hash <- eitherHash 
        salt <- eitherSalt
        Right . validatePassword hash $ append (pack pass) salt
