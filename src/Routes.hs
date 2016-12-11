{-# LANGUAGE ScopedTypeVariables #-}

module Routes where

import Happstack.Server
import Control.Monad (msum)

import Template

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

routes :: ServerPart Response
routes = do
    decodeBody myPolicy
    msum [ 
           dir "css"     $ css
         , homePage
         ]

css = path $ \(cssRequest :: String) -> 
             case cssRequest of
                "styles.css" -> ok $ (toResponse mainStyleSheet) 
                                       {rsHeaders=(mkHeaders [("Content-Type", "text/css")])}
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)
