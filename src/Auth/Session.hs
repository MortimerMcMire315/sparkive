{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, MultiParamTypeClasses #-}

module Auth.Session ( SessionServerPart
                    , SessionData(..)
                    , getToken
                    , putToken
                    , getUltDest
                    , putUltDest
                    ) where

import Control.Monad.Trans.Class      ( MonadTrans         )
import Control.Monad.IO.Class         ( liftIO             )
import Happstack.Server.ClientSession ( ClientSession(..)
                                      , ClientSessionT
                                      , MonadClientSession )
import Happstack.Server               ( ServerPartT
                                      , Response           )
import Data.ByteString                ( ByteString         )
import Data.Data                      ( Data, Typeable     )
import Data.SafeCopy                  ( base
                                      , deriveSafeCopy     )
import DB.Types                       ( DBConn             )

import qualified DB.Query as Query
import qualified Happstack.Server.ClientSession as CS

--This will be the type of all view functions
type SessionServerPart a = ClientSessionT SessionData (ServerPartT IO) a

data SessionData = SessionData { token :: Maybe ByteString
                               , username :: Maybe String
                               , ultDest :: Maybe String
                               }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SessionData)

instance ClientSession SessionData where
    emptySession = SessionData { token = Nothing
                               , username = Nothing
                               , ultDest = Nothing
                               }

getSession :: SessionServerPart SessionData
getSession = CS.getSession -- Doing this just to avoid having to inline-type
                           -- getSession every time.

getThing :: (SessionData -> a) -> SessionServerPart a
getThing f = fmap f getSession

putThing :: (SessionData -> SessionData) -> SessionServerPart ()
putThing newSession = getSession >>= (CS.putSession . newSession)

getToken :: SessionServerPart (Maybe ByteString)
getToken = getThing token

putToken :: String -> ByteString -> DBConn -> SessionServerPart (Either String ())
putToken uname tok conn = do
    putThing (\sess -> sess {token = Just tok, username = Just uname})
    liftIO $ Query.insertSessToken uname tok conn

getUltDest :: SessionServerPart (Maybe String)
getUltDest = getThing ultDest

putUltDest :: Maybe String -> SessionServerPart ()
putUltDest x = putThing (\sess -> sess {ultDest = x})
