{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, MultiParamTypeClasses #-}

module Auth.Session ( SessionServerPart
                    , SessionData(..)
                    , getToken
                    , putToken
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

newtype SessionData = SessionData { token :: Maybe ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SessionData)

instance ClientSession SessionData where
    emptySession = SessionData { token = Nothing }

getSession :: SessionServerPart SessionData
getSession = CS.getSession -- Doing this just to avoid having to inline-type
                           -- getSession every time.

getToken :: SessionServerPart (Maybe ByteString)
getToken = do
    sess <- getSession
    return (token sess) 

putToken :: Maybe ByteString -> SessionServerPart ()
putToken x = do
    sess <- getSession
    CS.putSession $ sess {token = x}
