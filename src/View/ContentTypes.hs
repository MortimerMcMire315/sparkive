module View.ContentTypes where
import Happstack.Server

-- |All MIME types that are used in this application
data MIMEType = CSS | JS | HTML

-- |Convert a MIMEType to a string.
getMimeString :: MIMEType -> String
getMimeString ct = case ct of
                     CSS    -> "text/css"
                     JS     -> "text/javascript"
                     HTML   -> "text/html"

-- |Convert the given data to a response, and add a corresponding Content-Type header
toResMime :: ToMessage a => a -> MIMEType -> Response
toResMime dat mimeType = (toResponse dat) {rsHeaders = mkHeaders [("Content-Type", getMimeString mimeType)]}
