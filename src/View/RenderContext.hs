module View.RenderContext where
import Text.Blaze.Html ( Html
                       , toHtml )

data UserInfo = LoggedOut | LoggedIn { username :: String } deriving Show

data RenderContext = RenderContext { user            :: UserInfo
                                   , errors          :: [String]
                                   , warnings        :: [String]
                                   , msgs            :: [String]
                                   , successContent  :: Html
                                   }

emptyRenderContext :: RenderContext
emptyRenderContext = RenderContext LoggedOut [] [] [] (toHtml "")

errorRenderContext :: String -> RenderContext
errorRenderContext e = emptyRenderContext {errors = [e]}

addError :: RenderContext -> String -> RenderContext
addError rc e = rc {errors = e:errors rc}

contentOrErrors :: RenderContext -> Html -> RenderContext
contentOrErrors rc content = if null $ errors rc
                             then rc {successContent = content}
                             else rc {successContent = toHtml ""}

contentIfNoErrors :: RenderContext -> RenderContext
contentIfNoErrors rc = if null $ errors rc
                       then rc
                       else rc {successContent = toHtml ""}
