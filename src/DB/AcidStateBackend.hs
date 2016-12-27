{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module DB.AcidStateBackend where

import Data.ByteString (ByteString)
import Data.Acid
import Data.Default (Default, def)
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)


data User = User { username   :: String
                 , hashedPass :: ByteString
                 , salt       :: ByteString
                 }

newtype Attribute = Attribute { name :: String }

data Item = Item { title    :: String
                 , dataPath :: FilePath
                 }

data Archive = Archive { users       :: [User]
                       , attributes  :: [Attribute]
                       , items       :: [Item]
                       , initialized :: Bool
                       }

instance Default Archive where
    def = Archive [] [] [] False

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''Archive)

addUser :: User -> Update Archive ()
addUser u = get >>= (\db -> let us = users db in
                        put $ db { users = u:us }
                    )

getUserByName :: String -> Query Archive User
getUserByName name = do
    db <- ask
    return . head $ users db

$(makeAcidic ''Archive ['addUser, 'getUserByName])
