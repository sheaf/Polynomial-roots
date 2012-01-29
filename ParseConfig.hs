module ParseConfig where

import Control.Exception(SomeException,IOException,handle)
import Data.Char(toLower)
import Data.Complex
import System.Environment(getArgs)
import System.IO
import Control.Applicative
import Control.Monad
import Data.Monoid

import Types
import Plotting

{-
parse :: (Read a) => String -> Maybe a
parse x
    | null y = Nothing
    | otherwise = Just $ fst (head y)
        where y = reads x

-- TODO: parse gradient parameters
parseToGradient str = case words str of
    (n:_) -> Just n
    _     -> Nothing
    
parseToMode :: String -> Maybe Mode
parseToMode s
    | null s = Nothing
    | s1 == ["roots"] = Just Roots
    | s1 == ["ifs"] = Just IFS
    | s1 == ["both"] = Just Both
    | otherwise = Nothing
    where s1 = map (map toLower) (take 1 $ words s)

parseMConfig :: (Coefficient a) => [String] -> Maybe (Mode, Config m a)
parseMConfig (m:ic:res:d:c:w:g:[]) = (,,) <$> mode <*> cfg
    where mode = parseToMode m
          cfg = Config <$> parse ic <*> parse res <*> parse d <*> parse c <*> parse w <*> parseToGradient g
parseMConfig _ = Nothing

askParse :: String -> (String -> Maybe a) -> IO a
askParse prompt parse = do putStrLn prompt
                           r <- parse <$> getLine
                           maybe parseFail return r
  where parseFail = do putStrLn "Unable to parse, try again." 
                       askParse prompt parse

askConfig :: (Coefficient a) => IO (Mode, String, Config m a)
askConfig = (,,) <$> askMode <*> askGrad <*> askCfg

askMode :: IO Mode
askMode = askParse "Enter desired mode. (roots/ifs/both)." parseToMode

askGrad :: IO String
askGrad = askParse "Enter gradient name." parseToGradient

askCfg :: (Coefficient a) => IO (Config m a)
askCfg = Config 
    <$> askParse "Enter desired coefficient set, e.g. [1,-1]." parse
    <*> askParse "Enter desired resolution, e.g. (400,400)." parse
    <*> askParse "Enter desired degree." parse
    <*> askParse "Enter desired center, e.g. 0:+(-0.707)." parse
    <*> askParse "Enter desired width." parse
-}

showConfig :: (Coefficient a) =>  Config m a -> IO()
showConfig (Config ic (rx,ry) d c w g)= do
    putStrLn("Coefficient set is "++show(ic)++".")
    putStrLn("Resolution is "++show(rx)++"x"++show(ry)++".")
    putStrLn("Degree is "++show(d)++".")
    putStrLn("Center is "++show(c)++".")
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    putStrLn("Width is "++show(w)++", height is "++show(h)++".")
    putStrLn("Gradient is: ???")


