{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module DB.AcidStateBackend ( insertUserQ
--                         , getUserByNameQ
                           , getPassHashQ
                           , Archive
                           ) where

import Control.Exception (throw)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid
import Data.ByteString (ByteString)
import Data.Default (Default, def)
import qualified Data.Map.Lazy as M
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Exception.Handler as E
import Exception.Util (handles)

newtype PassData = PassData { hashedPass :: ByteString }

newtype Users = Users { userMap :: M.Map String PassData }


newtype Attribute = Attribute { name :: String }

data Item = Item { title    :: String
                 , dataPath :: FilePath
                 }

data Archive = Archive { users       :: Users
                       , attributes  :: [Attribute]
                       , items       :: [Item]
                       , initialized :: Bool
                       }

instance Default Archive where
    def = Archive (Users M.empty) [] [] False

$(deriveSafeCopy 0 'base ''PassData)
$(deriveSafeCopy 0 'base ''Users)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''Archive)

insertUser :: String -> ByteString -> Update Archive ()
insertUser u passHash = do
    db <- get
    let (Users map) = users db
    let insertF = Users $ M.insert u (PassData passHash) map
    case M.lookup u map of
        Just otherUser -> throw $ E.UsernameTakenException errStr
        Nothing -> put $ db { users = insertF }
  where errStr = "Error: The username \"" ++ u ++ "\" is already taken."

--getUserByName :: String -> Query Archive (Maybe User)
--getUserByName name = do
--    db <- ask
--    return $ lookup name (users db)

getPassHash :: String -> Query Archive (Either String ByteString)
getPassHash name = do
    db <- ask
    let umap = userMap $ users db
    let maybeUser = M.lookup name umap
    return $ case maybeUser of
                Just u -> Right $ hashedPass u
                Nothing -> Left $ "Error: User " ++ name ++ " does not exist."

$(makeAcidic ''Archive ['insertUser, 'getPassHash])


{--- ACTUAL QUERIES ---}
insertUserQ :: String -> ByteString -> AcidState Archive -> IO (Either String ())
insertUserQ u passHash a = handles [E.handleUsernameTakenException] $ 
    fmap Right . update a $ InsertUser u passHash

getPassHashQ :: String -> AcidState Archive -> IO (Either String ByteString)
getPassHashQ u a = query a $ GetPassHash u
