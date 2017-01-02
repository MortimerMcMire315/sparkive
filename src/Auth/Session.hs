{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, MultiParamTypeClasses #-}

module Auth.Session ( SessionServerPart
                    , SessionData(..)
                    , getToken
                    , putToken
                    , getUltDest
                    , putUltDest
                    ) where

import Control.Monad.Trans.Class (MonadTrans)
import Happstack.Server.ClientSession ( ClientSession(..)
                                      , ClientSessionT
                                      , MonadClientSession )
import Happstack.Server               ( ServerPartT
                                      , Response           )
import Data.ByteString                ( ByteString         )
import Data.Data                      ( Data, Typeable     )
import Data.SafeCopy                  ( base
                                      , deriveSafeCopy     )

import qualified Happstack.Server.ClientSession as CS

--This will be the type of all view functions
type SessionServerPart a = ClientSessionT SessionData (ServerPartT IO) a

data SessionData = SessionData { token :: Maybe ByteString
                               , ultDest :: Maybe String
                               }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SessionData)

instance ClientSession SessionData where
    emptySession = SessionData { token = Nothing
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

putToken :: Maybe ByteString -> SessionServerPart ()
putToken x = putThing (\sess -> sess {token = x})

getUltDest :: SessionServerPart (Maybe String)
getUltDest = getThing ultDest

putUltDest :: Maybe String -> SessionServerPart ()
putUltDest x = putThing (\sess -> sess {ultDest = x})
