module View.Util ( nullDirServe
                 , tryQuery
                 , withConn
                 , withConnErrBox
                 , EDBConn        ) where

import Happstack.Server  ( ok
                         , toResponse
                         , nullDir
                         , ServerPart
                         , Response
                         , ToMessage  )
import Text.Hamlet       ( Html       )

import View.ContentTypes ( toResMime
                         , MIMEType   )
import DB.Types          ( DBConn     )
import qualified View.Template as T

type EDBConn = Either String DBConn

-- |Make sure the request is sane (no path segments after *.css or *.js or
--  whatever); if so, serve the file with MIME type "text/css"
nullDirServe :: (ToMessage t) => t -> MIMEType -> ServerPart Response
nullDirServe template mimeT = nullDir >> ok (toResMime template mimeT)

withConn :: EDBConn -> (String -> IO a) -> (DBConn -> IO a) -> IO a
withConn eitherConn failAction successAction =
    case eitherConn of
        Left err -> failAction err
        Right conn -> successAction conn

withConnErrBox :: EDBConn -> (DBConn -> IO Html) -> IO Html
withConnErrBox eitherConn = withConn eitherConn (return . T.errBoxT)

-- |Attempt to run a SQL query given a 'DBConn', a query function from 'Query',
-- and a function to run on success. The success function uses the results of
-- the query to construct an HTML fragment. On failure, uses 'T.errBoxT'
-- to display an error on the frontend.
tryQuery :: DBConn -> (DBConn -> IO (Either String a)) -> (a -> IO Html) -> IO Html
tryQuery conn queryF successAction = do
    eitherErrResults <- queryF conn
    case eitherErrResults of
        Left err        -> return $ T.errBoxT err
        Right results   -> successAction results
