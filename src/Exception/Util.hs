module Exception.Util
    ( handles
    ) where

import Control.Monad.Catch ( MonadCatch
                           , Handler
                           , catches  )

-- |'catches' with the arguments flipped.
handles :: (Foldable f, MonadCatch m) => f (Handler m a) -> m a -> m a
handles = flip catches
