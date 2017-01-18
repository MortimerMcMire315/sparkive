{-# LANGUAGE ScopedTypeVariables #-}

module Exception.Util
    ( handles
    , rethrowIO
    ) where

import Control.Monad.Catch ( Exception
                           , Handler
                           , MonadCatch
                           , catch
                           , catches  
                           , throwM    )

-- |'catches' with the arguments flipped.
handles :: (Foldable f, MonadCatch m) => f (Handler m a) -> m a -> m a
handles = flip catches

-- |Catch an IOError, and rethrow it as another kind of error.
rethrowIO :: (MonadCatch m, Exception e) => 
             m a                         -- ^ A throwing IO action to perform
          -> (String -> e)               -- ^ An exception constructor (probably from Exception.Handler)
          -> Handler m (Either String a) -- ^ An exception handler (probably also from Exception.Handler)
          -> String                      -- ^ Error string (user-friendly error info) 
          -> m (Either String a)         -- ^ Returns either an error string or the result of the action
rethrowIO action constructor handler errStr = 
    catches ( catch (fmap Right action) (\(x :: IOError) -> throwM $ constructor errStr)) [handler]
