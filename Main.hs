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
module Main where

import Control.Applicative
import Control.Exception(SomeException,IOException,handle)
import Data.Complex
import System.Environment(getArgs)
import System.IO
import Data.Char
import Data.Maybe

import Roots
import IFS
import Util
import Types
import Plotting
import Interval
import Image
import ParseConfig
import MainGUI
import Rendering.Raster

ifsRoutine :: (Integral v, Num a, Coefficient a) 
           => Config (Sum v) GDColor a -> IO()
ifsRoutine cfg = do
    rst <- createRasterizerIO ifsFunc cfg
    putStrLn ""
    putStrLn "IFS routine."
    putStrLn "Computing scale factors... (experimental)"
    putStrLn $ "Scale factors are: " ++ show (getScales cfg)
    putStrLn "Computing IFS..."
    putStrLn $ "I'm going to write to file '" ++ ifsfile ++ "'."
    let pxs = rasterize rst <$> ifsPoints cfg
    writeImageFile cfg ifsfile pxs
    putStrLn "Done writing to file 'ifs_image.png'. Finished IFS routine."
  where ifsfile = "ifs_image.png"
        ifsFunc (IFSPlot p) = (complexToPair p, Sum 100)

rootsRoutine :: (Integral v, Real a, Coefficient a) 
             => Config (Sum v) GDColor a -> IO()
rootsRoutine cfg = do
    rst <- createRasterizerIO rootFunc cfg
    putStrLn ""
    putStrLn "Roots routine."
    putStrLn "Computing roots."
    putStrLn $ "I'm going to write to file '" ++ rootsfile ++ "'."
    let pxs = rasterize rst <$> getRoots cfg
    writeImageFile cfg rootsfile pxs
    putStrLn "Done writing to file 'roots_image.png'. Finished roots routine."
  where rootsfile = "roots_image.png"
        rootFunc (RootPlot _ r) = (complexToPair r, Sum 25)

writeImageFile :: (Monoid v) => Config v GDColor a -> FilePath 
               -> [IO (Maybe (RstCoord, v))] -> IO ()
writeImageFile cfg fn pts = writeImage fn pts res g
  where res = resolution cfg
        g = gradient cfg

main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn ""
    handleOptions

runAsCmd :: (Integral v) => (Mode, Config (Sum v) GDColor Int) -> IO ()
runAsCmd (mode, cfg) = do 
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

runAsGui :: (Integral v) => (Mode, Config (Sum v) RGB8 Int) -> IO ()
runAsGui (mode, cfg) = do
    showConfig cfg
    putStrLn "    Starting GUI..."
    guiMain mode cfg

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


--this should be changed to only accept exception: file doesn't exist...
alwaysError :: IOException -> IO(Maybe a)
alwaysError = \_ -> return Nothing

parseConfigFile :: (Integral v, RGB c, Coefficient a) 
                => IO (Maybe (Mode, Config (Sum v) c a))
parseConfigFile = do
    file' <- handle (alwaysError) ((fmap Just) $ (openFile "roots.ini" ReadMode))
    case file' of
        Nothing -> return Nothing
        Just file -> do strings <- (fmap lines) $ hGetContents file
                        return $ parseMConfig (take 7 strings)

handleOptions :: IO ()
handleOptions = do
    args <- getArgs
    case args of
        ("gui":args') -> parseGuiArgs $ parseMConfig args'
        _             -> parseCmdArgs $ parseMConfig args

parseGuiArgs :: (Integral v) => Maybe (Mode, Config (Sum v) RGB8 Int) -> IO ()
parseGuiArgs (Just cfg) = putStrLn "Using command line args." >> runAsGui cfg
parseGuiArgs Nothing    = do 
    cfg' <- parseConfigFile
    case cfg' of
        Nothing  -> putStrLn "No valid config file found. (roots.ini)"
        Just cfg -> runAsGui cfg

parseCmdArgs :: (Integral v) => Maybe (Mode, Config (Sum v) GDColor Int) -> IO ()
parseCmdArgs (Just cfg) = putStrLn "Using command line args." >> runAsCmd cfg
parseCmdArgs Nothing    = do 
    cfg' <- parseConfigFile
    case cfg' of
        Nothing -> do 
            putStrLn "No valid config file found. (roots.ini)"
            runAsCmd =<< askConfig
        Just cfg -> do
            ans <- askYN "Valid config file found. Use config file? (y/n) "
            if ans then runAsCmd cfg else askAboutEverything


askAboutEverything = do useGUI <- askYN "Run as GUI? (y/n)"
                        if useGUI 
                            then runAsGui =<< askConfig 
                            else runAsCmd =<< askConfig 



