module Server where

import Happstack.Server (simpleHTTP, nullConf)

import Routes
import Model.Model (Archive)
import DB.Conn (getConn)
import qualified Config

run :: IO ()
run = do
    conn <- getConn
    simpleHTTP nullConf $ routes conn
