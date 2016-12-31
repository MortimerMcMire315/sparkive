module DB.Types ( PostgresAuth(..)
                , DBInfo(..)
                , DBConn(..)
                ) where

import Database.PostgreSQL.Simple ( Connection )
import Data.Acid                  ( AcidState  )

import DB.AcidStateBackend        ( Archive    )

data PostgresAuth = PostgresAuth { host   :: String
                                 , user   :: String
                                 , pass   :: String
                                 , port   :: String
                                 , dbname :: String } deriving Show

data DBInfo = AcidStateInfo { dir :: FilePath} | PostgresInfo PostgresAuth

data DBConn = AcidStateConn { acidStateDir   :: FilePath
                            , archive        :: AcidState Archive
                            }
            | PostgresConn  { pgUser :: String
                            , conn   :: Connection
                            }
