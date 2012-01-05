module ParseConfig where

import Control.Exception(SomeException,IOException,handle)
import Data.Char(toLower)
import Data.Complex
import System.Environment(getArgs)
import System.IO
import Control.Applicative
import Control.Monad

import Types
import Plotting

parse :: (Read a) => String -> Maybe a
parse x
    | null y = Nothing
    | otherwise = Just $ fst (head y)
        where y = reads x

parseToGradient :: (RGB c) => String -> Maybe (Gradient c)
parseToGradient "" = Nothing
parseToGradient s
    | s1 == ["warm"] = Just(warm h)
    | s1 == ["cold"] = Just(cold h)
    | s1 `elem` [["grey"],["gray"]] = Just(grey h)
    | s1 `elem` [["bw"], ["binary"], ["01"], ["10"]] = Just(binary h)
    | otherwise = Nothing
        where s' = words s
              s1 = map (map toLower) $ take 1 s'
              s2 = map parse ( take 1 $ drop 1 s' ) ::[Maybe Int]
              h = case s2 of
                     [Just h'] -> h'
                     otherwise -> 12 --random value

parseToMode :: String -> Maybe Mode
parseToMode s
    | null s = Nothing
    | s1 == ["roots"] = Just Roots
    | s1 == ["ifs"] = Just IFS
    | s1 == ["both"] = Just Both
    | otherwise = Nothing
    where s1 = map (map toLower) (take 1 $ words s)

parseMConfig :: (RGB c, Coefficient a) => [String] -> Maybe (Mode, Config c a)
parseMConfig (m:ic:res:d:c:w:g:[]) = (,) <$> mode <*> cfg
    where mode = parseToMode m
          grad = parseToGradient g
          cfg = Config <$> parse ic <*> parse res <*> parse d <*> parse c <*> parse w <*> grad
parseMConfig _ = Nothing

askParse :: String -> (String -> Maybe a) -> IO a
askParse prompt parse = do putStrLn prompt
                           r <- parse <$> getLine
                           maybe parseFail return r
  where parseFail = do putStrLn "Unable to parse, try again." 
                       askParse prompt parse

askConfig :: (RGB c, Coefficient a) => IO (Mode,Config c a)
askConfig = (,) <$> askMode <*> askCfg

askMode :: IO Mode
askMode = askParse "Enter desired mode. (roots/ifs/both)." parseToMode

askCfg :: (RGB c, Coefficient a) => IO (Config c a)
askCfg = Config 
    <$> askParse "Enter desired coefficient set, e.g. [1,-1]." parse
    <*> askParse "Enter desired resolution, e.g. (400,400)." parse
    <*> askParse "Enter desired degree." parse
    <*> askParse "Enter desired center, e.g. 0:+(-0.707)." parse
    <*> askParse "Enter desired width." parse
    <*> askParse "Enter desired gradient. (warm/cold/grey/binary)" parseToGradient

showConfig :: Coefficient a =>  Config c a -> IO()
showConfig (Config ic (rx,ry) d c w grad)= do
    putStrLn("Coefficient set is "++show(ic)++".")
    putStrLn("Resolution is "++show(rx)++"x"++show(ry)++".")
    putStrLn("Degree is "++show(d)++".")
    putStrLn("Center is "++show(c)++".")
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    putStrLn("Width is "++show(w)++", height is "++show(h)++".")
    putStrLn("Gradient is "++showGradient(grad)++".")

