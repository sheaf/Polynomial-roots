module Util where

import Control.Monad (liftM)
import System.Directory (doesFileExist)
import System.IO (print)

unfoldActionM :: (Monad m) => m (Maybe a) -> m [a]
unfoldActionM mx = step =<< mx
    where step (Just x) = liftM (x:) (unfoldActionM mx)
          step Nothing = return []

printParseError :: String -> IO()
printParseError s = do putStrLn "Parse error:"
                       putStrLn s
                       putStrLn "Terminating application."

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x s = and . map (uncurry (==)) $ zip (reverse x) (reverse s)

nextImageName :: (Int -> String) -> IO(String)
nextImageName f = go f 1
    where go f n = do exists <- doesFileExist (f n)
                      if exists then go f (n+1) else return (f n)
