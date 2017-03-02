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

import qualified View.RenderContext as RC

-- |styles.css, the main stylesheet for the site.
mainStyleSheet = renderCss $ $(luciusFile (cssFile "styles")) undefined

-- |create-db-button.js
createDBButtonJS = renderJavascriptUrl (\_ _ -> undefined) $(juliusFile (jsFile "create-db-button"))

-- |The banner that runs across the top of the main page.
mainPageBannerT rc = $(shamletFile $ hamFile "main-page-banner")

-- |The <head> section to go on every page.
headerT         = $(shamletFile $ hamFile "header")

-- |Not really sure what this will be used for, actually.
footerT         = $(shamletFile $ hamFile "footer")

errBoxRawHtml :: String -> Html
errBoxRawHtml = errBoxT . RC.errorRenderContext

-- |Display red error boxes for each error in the provided render context
errBoxT :: RC.RenderContext -> Html
errBoxT rc = let errStrs = map preEscapedToHtml (RC.errors rc) in
                             $(shamletFile $ hamFile "err-box")

-- |A <div> displaying generic query results.
genericResultT :: [[String]] -> Html
genericResultT results = $(shamletFile $ hamFile "generic-result")

-- |A wrapper for every page.
genericAdminPageT :: RC.RenderContext -> Html -> Html
genericAdminPageT rc content = $(shamletFile $ hamFile "generic-admin-page")

createDBButtonT :: Html
createDBButtonT = $(shamletFile $ hamFile "create-db-button")

loginPageT :: RC.RenderContext -> Html
loginPageT rc = genericAdminPageT rc $ $(shamletFile $ hamFile "login")

homePageT :: RC.RenderContext -> Html
homePageT rc = genericAdminPageT rc $ $(shamletFile $ hamFile "home")

adminPanelT :: RC.RenderContext -> Html
adminPanelT rc = genericAdminPageT rc $ $(shamletFile $ hamFile "admin-panel")
