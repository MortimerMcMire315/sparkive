{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Server where

import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (toHtml)
import Text.Hamlet (shamletFile, Html)
import Text.Lucius (luciusFile, renderCss, Css)
import TemplateUtil (hamFile, cssFile)

import Data.List.Split (splitOn)
import Data.List (intercalate)

run :: IO ()
run = simpleHTTP nullConf starchive

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

starchive :: ServerPart Response
starchive = do
    decodeBody myPolicy
    msum [ 
           dir "css"     $ css
         , homePage
         ]

css = path $ \(cssRequest :: String) -> 
             case cssRequest of
                "styles.css" -> ok $ (toResponse mainStyleSheet) {rsHeaders=(mkHeaders [("Content-Type", "text/css")])}
                _            -> notFound $ toResponse ("CSS stylesheet not found." :: String)

{--
serveLuciusFile str
    | head parts /= "css" = notFound "CSS resource could not be found."
    | otherwise = ok . toResponse . getLuciusFile . reverse $ intercalate "." toLucius
    where parts = reverse $ splitOn "." str
          toLucius = "css" : tail parts
          --}

mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

header :: Html
header = $(shamletFile $ hamFile "header")

footer :: Html
footer = $(shamletFile $ hamFile "footer")

homePage :: ServerPart Response
homePage = ok $ toResponse ( $(shamletFile $ hamFile "home") )
