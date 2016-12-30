{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module DB.AcidStateBackend ( insertUserQ
--                         , getUserByNameQ
                           , getPassHashQ
                           , Archive
                           ) where

import Control.Exception     ( throw           )
import Control.Monad.Catch   ( throwM          )
import Control.Monad.Reader  ( ask             )
import Control.Monad.State   ( get, put        )
import Data.Acid             ( AcidState
                             , Update
                             , Query
                             , update
                             , query
                             , makeAcidic      )
import Data.ByteString       ( ByteString      )
import Data.ByteString.Char8 ( pack            )
import Data.Default          ( Default
                             , def             )
import Data.SafeCopy         ( base
                             , deriveSafeCopy  )

import qualified Data.Map.Lazy as M

import qualified Exception.Handler as E
import Exception.Util ( handles )

data User = User { username   :: String
                 , hashedPass :: ByteString
                 , salt       :: ByteString
                 }

newtype Users = Users { userMap :: M.Map String User }


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

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''Archive)

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

$(makeAcidic ''Archive ['insertUser, 'getUserByName])


{--- ACTUAL QUERIES ---}
insertUserQ :: String -> ByteString -> AcidState Archive -> IO (Either String ())
insertUserQ u passHash a = handles [E.handleUsernameTakenException] $
    fmap Right . update a $ InsertUser $ User u passHash (pack "")

getPassHashQ :: String -> AcidState Archive -> IO (Either String ByteString)
getPassHashQ u a = do
    maybeUser <- query a $ GetUserByName u
    return $ case maybeUser of
                Just theUser -> Right $ hashedPass theUser
                Nothing -> Left $ "Error: User " ++ u ++ " not found."
