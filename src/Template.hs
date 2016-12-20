{-# LANGUAGE TemplateHaskell #-}
module Template where

import Happstack.Server
import Text.Hamlet (shamletFile, Html)
import Text.Lucius (luciusFile, renderCss, Css)

import TemplateUtil (hamFile, cssFile)

{-- LUCIUS PAGES --}
mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

{-- HAMLET PAGES --}
mainPageBanner = $(shamletFile $ hamFile "mainPageBanner")
header         = $(shamletFile $ hamFile "header")
footer         = $(shamletFile $ hamFile "footer")
homePage :: [[String]] -> String -> Html
homePage confResult err = $(shamletFile $ hamFile "home")
