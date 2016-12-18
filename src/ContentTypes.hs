module ContentTypes where
import Happstack.Server

--Really could just use strings, but doing it this way gives compile-time safety.
data MIMEType = CSS | HTML

--Convert a MIMEType to a string.
getMimeString :: MIMEType -> String
getMimeString ct = case ct of
                     CSS    -> "text/css"
                     HTML   -> "text/html"

--Convert the given data to a response, and add a corresponding Content-Type header
toResMime :: ToMessage a => a -> MIMEType -> Response
toResMime dat mimeType = (toResponse dat) {rsHeaders=(mkHeaders [("Content-Type", getMimeString mimeType)])}

