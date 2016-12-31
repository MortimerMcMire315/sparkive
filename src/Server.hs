module Server where

import Happstack.Server               ( simpleHTTP
                                      , nullConf       )
import Happstack.Server.ClientSession ( getDefaultKey
                                      , sessionPath
                                      , withClientSessionT
                                      , mkSessionConf  )
import Routes                         ( routes         )
import DB.Conn                        ( getConn        )

run :: IO ()
run = do
    conn <- getConn
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    simpleHTTP nullConf $ withClientSessionT sessionConf $ routes conn
