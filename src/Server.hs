module Server where

import Happstack.Server (simpleHTTP, nullConf)

import Routes

run :: IO ()
run = simpleHTTP nullConf routes
