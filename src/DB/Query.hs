{-# LANGUAGE OverloadedStrings #-}

module DB.Query where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple

--checkDBExists :: Connection -> IO Bool
--checkDBExists conn = do
--    results <- (query_ conn "SELECT * FROM attribute" :: IO [[String]])
--    return True
