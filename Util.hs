module Util where

import Control.Monad (liftM)

unfoldActionM :: (Monad m) => m (Maybe a) -> m [a]
unfoldActionM mx = step =<< mx
    where step (Just x) = liftM (x:) (unfoldActionM mx)
          step Nothing = return []
