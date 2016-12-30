module Server where

import Happstack.Server ( simpleHTTP
                        , nullConf   )
import Routes           ( routes     )
import DB.Conn          ( getConn    )

run :: IO ()
run = do
    conn <- getConn
    simpleHTTP nullConf $ routes conn
