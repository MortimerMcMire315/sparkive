{-# LANGUAGE TemplateHaskell #-}
module Template where

import Happstack.Server
import Text.Hamlet (shamletFile, Html)
import Text.Lucius (luciusFile, renderCss, Css)
import Text.Blaze.Html (preEscapedToHtml)

import TemplateUtil (hamFile, cssFile)

{-- LUCIUS PAGES --}
mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

{-- HAMLET PAGES --}
mainPageBannerT = $(shamletFile $ hamFile "mainPageBanner")
headerT         = $(shamletFile $ hamFile "header")
footerT         = $(shamletFile $ hamFile "footer")

errBoxT :: String -> Html
errBoxT err = let errStr = preEscapedToHtml err in
                             $(shamletFile $ hamFile "errBox")

homePageT :: [[String]] -> String -> Html
homePageT confResult err = let errBox = errBoxT err in
                                 $(shamletFile $ hamFile "home")
