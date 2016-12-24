module Routes 
    ( routes
    ) where

import Happstack.Server
import Control.Monad (msum)

import qualified View.Views as Views

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- |Happstack routing function.
--  /css/* -> 'Views.serveCSS'
--  /      -> 'Views.homePage'
routes :: ServerPart Response
routes = do
    decodeBody myPolicy
    msum [ 
           dir "css"        Views.serveCSS
         , nullDir       >> Views.homePage
         ]

