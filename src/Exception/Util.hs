module Exception.Util (handles) where

import Control.Monad.Catch (MonadCatch, Handler, catches)

handles :: (Foldable f, MonadCatch m) => f (Handler m a) -> m a -> m a
handles ls action = catches action ls
