{- Polynomial roots.
Computes the set of polynomials with a particular restricted set of coeffs,
who might have roots in a particular rectangle in the complex plane,
using tree pruning.

To do:
1) Fix parsing (choice of type of coefficients).
2) polySolve not allowing complex coefficients.
3) Optimisation.
4) Investigate better bounds. 
5) Clean up gradients, and make some nicer ones (sunset purple-orange-yellow).-}

{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Exception(SomeException,IOException,handle)
import Data.Char(toLower)
import Data.Complex
import Data.List
import Data.Maybe
import System.Environment(getArgs)
import System.IO

import Roots
import IFS
import Types
import Plotting
import Interval
import Image

--------------------------------------------------------------------------------
--Parsing options.

--Basic parsing.
parse :: (Read a) => String -> Maybe a
parse x
    | null y = Nothing
    | otherwise = Just $ fst (head y)
        where y = reads(x)

parseToGradient :: String -> Maybe Gradient
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

askYN :: String -> IO(Bool)
askYN s = do
    putStrLn s
    ans' <- getLine
    let ans = map toLower ans'
    case ans of
        "y" -> return True
        "n" -> return False
        _ -> do putStrLn "Sorry? "
                askYN s

askMode :: String -> IO(Mode)
askMode s = do
    putStrLn s
    ans' <- getLine
    let ans = map toLower ans'
    case ans of
        "ifs" -> return IFS
        "roots" -> return Roots
        "both" -> return Both
        _ -> do putStrLn "Sorry? "
                askMode s

parseMConfig :: Coefficient a => [String] -> Maybe (Mode,Config a)
parseMConfig (m:ic:res:d:c:w:g:[]) = (,) <$> mode <*> cfg
    where mode = parseToMode m
          grad = parseToGradient g
          cfg = Config <$> parse ic <*> parse res <*> parse d <*> parse c <*> parse w <*> grad
parseMConfig _ = Nothing

--this should be changed to only accept exception: file doesn't exist...
alwaysError :: IOException -> IO(Maybe a)
alwaysError = \_ -> return Nothing

parseConfigFile :: Coefficient a => IO(Maybe (Mode,Config a))
parseConfigFile = do
    file' <- handle (alwaysError) ((fmap Just) $ (openFile "roots.ini" ReadMode))
    case file' of
        Nothing -> return Nothing
        Just file -> do strings <- (fmap lines) $ hGetContents file
                        return $ parseMConfig (take 7 strings)

parseOptions :: IO((Mode,Config Int))
parseOptions = do
    args' <- fmap parseMConfig getArgs
    case args' of
        Just args -> do
            putStrLn "Using command line args."
            return args
        Nothing -> do
            cfg' <- parseConfigFile
            case cfg' of
                Nothing -> do 
                    putStrLn "No valid config file found. (roots.ini)"
                    askConfig
                Just cfg -> do
                    ans <- askYN "Valid config file found. Use config file? (y/n) "
                    if ans 
                        then return cfg
                        else askConfig 

askConfig :: Coefficient a => IO((Mode,Config a))
askConfig = do
    putStrLn "Enter desired mode. (roots/ifs/both). "
    m <- getLine
    putStrLn "Enter desired coefficient set, e.g. [1,-1]."
    ic <- getLine
    putStrLn "Enter desired resolution, e.g. (400,400). "
    res <- getLine
    putStrLn "Enter desired degree. "
    d <- getLine
    putStrLn "Enter desired center, e.g. 0:+(-0.707)."
    c <- getLine
    putStrLn "Enter desired width."
    w <- getLine
    putStrLn "Enter desired gradient. (warm/cold/grey/binary)"
    grad <- getLine
    let mconf'' = [m,ic,res,d,c,w,grad]
    let mconf' = parseMConfig mconf''
    case mconf' of
        Nothing -> do putStrLn "Unable to parse, try again."
                      askConfig
        Just mconf -> return mconf
    
showConfig :: Coefficient a =>  Config a -> IO()
showConfig (Config ic (rx,ry) d c w grad)= do
    putStrLn("Coefficient set is "++show(ic)++".")
    putStrLn("Resolution is "++show(rx)++"x"++show(ry)++".")
    putStrLn("Degree is "++show(d)++".")
    putStrLn("Center is "++show(c)++".")
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    putStrLn("Width is "++show(w)++", height is "++show(h)++".")
    putStrLn("Gradient is "++showGradient(grad)++".")

--------------------------------------------------------------------------------
--Main routines.

ifsRoutine :: Coefficient a => Config a -> IO()
ifsRoutine (Config ic (rx,ry) d c w g) = do
    let scales = scaleFactors (Config ic (rx,ry) (d+8) c (w/ (fromIntegral rx)) g)  
                    -- bear in mind scaleFactors uses w as an error bound...
    let ifs = toifs ic c
    --                                          
    let ifspixels = ifsCounts scales ifs (Config ic (rx,ry) (d+1) c w g)
    --let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    --let cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
    --let pols = canHaveRoots ic d cI
    --let ifspixels = ifsCheatCounts scales pols (Config ic (rx,ry) d c w g)
    --
    
    let ifsfile = "ifs_image.png"
    putStrLn ""
    putStrLn "IFS routine."
    putStrLn "Computing scale factors... (experimental)"
    putStrLn("Scale factors are: "++(show scales))
    putStrLn "Computing IFS..."
    putStrLn "I'm going to write to file 'ifs_image.png'."
    writeImage ifsfile ifspixels (rx,ry) g
    putStrLn "Done writing to file 'ifs_image.png'. Finished IFS routine."

rootsRoutine :: {-Coefficient a =>-} Config Int -> IO()
rootsRoutine (Config ic (rx,ry) d c w g) = do
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    let cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
    let polys = canHaveRoots ic d cI
    let rootspixels = rootList polys (rx,ry) c w
    let rootsfile = "roots_image.png"
    putStrLn ""
    putStrLn "Roots routine."
    putStrLn "Computing roots."
    putStrLn "I'm going to write to file 'roots_image.png'."
    writeImage rootsfile rootspixels (rx,ry) g 
    putStrLn "Done writing to file 'roots_image.png'. Finished roots routine."

main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn ""
    (mode,cfg) <- parseOptions
    putStrLn ""
    showConfig cfg
    case mode of
        Roots -> rootsRoutine cfg
        IFS -> ifsRoutine cfg
        Both -> do rootsRoutine cfg
                   ifsRoutine cfg
    putStrLn ""
    putStrLn "All done here! Press enter to quit."
    getLine
    putStrLn "Bye!"
