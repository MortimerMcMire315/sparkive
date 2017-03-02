module Server where

import Happstack.Server               ( simpleHTTP
                                      , nullConf       )
import Happstack.Server.ClientSession ( getDefaultKey
                                      , sessionPath
                                      , withClientSessionT
                                      , mkSessionConf  )
import Routes                         ( routes         )

run :: IO ()
run = do
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    simpleHTTP nullConf $ withClientSessionT sessionConf routes
