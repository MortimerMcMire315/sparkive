{-# LANGUAGE TemplateHaskell #-}
module View.Template where

import Happstack.Server
import Text.Hamlet (shamletFile, Html)
import Text.Lucius (luciusFile, renderCss, Css)
import Text.Julius (juliusFile, renderJavascriptUrl, Javascript)
import Text.Blaze.Html (preEscapedToHtml)

import View.TemplateUtil (hamFile, cssFile, jsFile)

-- |styles.css, the main stylesheet for the site.
mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

-- |create-db-button.js
createDBButtonJS = renderJavascriptUrl (\_ _ -> undefined) $(juliusFile (jsFile "create-db-button"))

-- |The banner that runs across the top of the main page.
mainPageBannerT = $(shamletFile $ hamFile "mainPageBanner")

-- |The <head> section to go on every page.
headerT         = $(shamletFile $ hamFile "header")

-- |Not really sure what this will be used for, actually.
footerT         = $(shamletFile $ hamFile "footer")

-- |A red error box that can be displayed to the user if any exception occurs.
errBoxT :: String -> Html
errBoxT err = let errStr = preEscapedToHtml err in
                             $(shamletFile $ hamFile "errBox")

-- |A <div> displaying generic query results.
genericResultT :: [[String]] -> Html
genericResultT results = $(shamletFile $ hamFile "genericResult")

createDBButtonT :: Html
createDBButtonT = $(shamletFile $ hamFile "createDBButton")

homePageT :: Html -> Html
homePageT toDisplay = $(shamletFile $ hamFile "home")
