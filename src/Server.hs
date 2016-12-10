{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Server where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

run :: IO ()
run = serve Nothing starchive

starchive :: ServerPart Response
starchive = msum
    [  
      homePage
    ]

--template :: Text -> ServerPart Response
template :: ToMessage a => a -> ServerPart Response
template text = ok $ toResponse text

--header :: ServerPart Response
header = $(shamletFile "src/Template/header.hamlet")

--footer :: ServerPart Response
footer = $(shamletFile "src/Template/footer.hamlet")

homePage :: ServerPart Response
homePage = template ( $(shamletFile "src/Template/home.hamlet") )
