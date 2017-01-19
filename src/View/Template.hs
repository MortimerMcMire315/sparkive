{-# LANGUAGE TemplateHaskell #-}
module View.Template where

import Text.Hamlet       ( shamletFile
                         , Html              )
import Text.Lucius       ( luciusFile
                         , renderCss
                         , Css               )
import Text.Julius       ( juliusFile
                         , renderJavascriptUrl
                         , Javascript        )
import Text.Blaze.Html   ( preEscapedToHtml  )

import View.TemplateUtil ( hamFile
                         , cssFile
                         , jsFile            )

-- |styles.css, the main stylesheet for the site.
mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

-- |create-db-button.js
createDBButtonJS = renderJavascriptUrl (\_ _ -> undefined) $(juliusFile (jsFile "create-db-button"))

-- |The banner that runs across the top of the main page.
mainPageBannerT = $(shamletFile $ hamFile "main-page-banner")

-- |The <head> section to go on every page.
headerT         = $(shamletFile $ hamFile "header")

-- |Not really sure what this will be used for, actually.
footerT         = $(shamletFile $ hamFile "footer")

-- |A red error box that can be displayed to the user if any exception occurs.
errBoxT :: String -> Html
errBoxT err = let errStr = preEscapedToHtml err in
                             $(shamletFile $ hamFile "err-box")

-- |A <div> displaying generic query results.
genericResultT :: [[String]] -> Html
genericResultT results = $(shamletFile $ hamFile "generic-result")

genericAdminPageT :: Html -> Html
genericAdminPageT content = $(shamletFile $ hamFile "generic-admin-page")

createDBButtonT :: Html
createDBButtonT = $(shamletFile $ hamFile "create-db-button")

loginPageT :: Html -> Html
loginPageT err = genericAdminPageT $ $(shamletFile $ hamFile "login")

homePageT :: Html -> Html
--the Html argument just makes for convenient development, and
--should be removed as this project matures.
homePageT toDisplay = genericAdminPageT $ $(shamletFile $ hamFile "home")

adminPanelT :: Html
adminPanelT = genericAdminPageT $ $(shamletFile $ hamFile "admin-panel")
