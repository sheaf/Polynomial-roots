module Util where

-- base
import Control.Monad
  ( liftM )

-- directory
import System.Directory
  ( doesFileExist )

--------------------------------------------------------------------------------

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
    where
      go :: (Int -> FilePath) -> Int -> IO FilePath
      go g n = do exists <- doesFileExist (g n)
                  if exists
                  then go g (n+1)
                  else return (g n)

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault f da db (a:as) (b:bs) = f a  b  : zipWithDefault f da db as bs
zipWithDefault f da db []     (b:bs) = f da b  : zipWithDefault f da db [] bs
zipWithDefault f da db (a:as) []     = f a  db : zipWithDefault f da db as []
zipWithDefault _ _ _   []     []     = []
