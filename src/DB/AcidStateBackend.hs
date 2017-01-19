{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module DB.AcidStateBackend ( insertUserQ
                           , insertSessTokenQ
                           , checkUserExistsQ
                           , getPassHashQ
                           , getSaltQ
                           , Archive
                           ) where

import Control.Exception     ( throw          )
import Control.Monad.Catch   ( throwM         )
import Control.Monad.Reader  ( ask            )
import Control.Monad.State   ( get, put       )
import Data.Acid             ( AcidState
                             , Update
                             , Query
                             , update
                             , query
                             , makeAcidic     )
import Data.ByteString       ( ByteString     )
import Data.ByteString.Char8 ( pack           )
import Data.Default          ( Default
                             , def            )
import Data.SafeCopy         ( base
                             , deriveSafeCopy )

import qualified Data.Map.Lazy as M

import qualified Exception.Handler as E
import Exception.Util ( handles )

data User = User { username   :: String
                 , hashedPass :: ByteString
                 , salt       :: ByteString
                 }

--Maps username to user info
newtype Users = Users { userMap :: M.Map String User }


newtype Attribute = Attribute { name :: String }

data Item = Item { title    :: String
                 , dataPath :: FilePath
                 }

--Maps username to token
newtype SessTokens = SessTokens { tokenMap :: M.Map String ByteString }

data Archive = Archive { users       :: Users
                       , attributes  :: [Attribute]
                       , items       :: [Item]
                       , initialized :: Bool
                       , tokens      :: SessTokens
                       }

instance Default Archive where
    def = Archive { users = Users M.empty
                  , attributes = []
                  , items = []
                  , initialized = False
                  , tokens = SessTokens M.empty
                  }

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 1 'base ''SessTokens)
$(deriveSafeCopy 1 'base ''Archive)

insertUser :: User -> Update Archive ()
insertUser u = do
    db <- get
    let (Users map) = users db
    let theName = username u
    let insertF = Users $ M.insert theName u map
    let errStr = "Error: The username \"" ++ theName ++ "\" is already taken."
    case M.lookup theName map of
        Just otherUser -> throw $ E.UsernameTakenException errStr
        Nothing -> put $ db { users = insertF }

getUserByName :: String -> Query Archive (Maybe User)
getUserByName name = do
    db <- ask
    let uMap = userMap $ users db
    return $ M.lookup name uMap

insertSessToken :: String -> ByteString -> Update Archive ()
insertSessToken uname tok = do
    db <- get
    let (SessTokens map) = tokens db
    let newTokens = SessTokens $ M.insert uname tok map
    put $ db { tokens = newTokens }

$(makeAcidic ''Archive ['insertUser, 'getUserByName, 'insertSessToken])


{--- ACTUAL QUERIES ---}
insertUserQ :: String -> ByteString -> ByteString -> AcidState Archive -> IO (Either String ())
insertUserQ u passHash salt a = handles [E.handleUsernameTakenException] $
    fmap Right . update a $ InsertUser $ User u passHash salt

insertSessTokenQ :: String -> ByteString -> AcidState Archive -> IO (Either String ())
insertSessTokenQ uname tok a = fmap Right . update a $ InsertSessToken uname tok

getUserAttrQ :: (User -> a) -> String -> AcidState Archive -> IO (Either String a)
getUserAttrQ f u a = do
    maybeUser <- query a $ GetUserByName u
    return $ case maybeUser of
                Just theUser -> Right $ f theUser
                Nothing -> Left $ "Error: User " ++ u ++ " not found."

getPassHashQ :: String -> AcidState Archive -> IO (Either String ByteString)
getPassHashQ = getUserAttrQ hashedPass

getSaltQ :: String -> AcidState Archive -> IO (Either String ByteString)
getSaltQ = getUserAttrQ salt

checkUserExistsQ :: String -> AcidState Archive -> IO (Either String Bool)
checkUserExistsQ u a = do
    maybeUser <- query a $ GetUserByName u
    return $ case maybeUser of
                Just theUser -> Right True
                Nothing      -> Right False
