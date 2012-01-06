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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception(IOException,handle)
import System.Environment(getArgs)
import System.IO
import Data.Char
import Data.Foldable (Foldable, toList)
import Data.Maybe
import Data.Monoid
import Roots
import IFS
import Util
import Types hiding (Gradient(..))
import Plotting
import Image
import ParseConfig
import MainGUI
import Pair
import Settings
import Configuration hiding (Roots, IFS)
import Configuration.Parsing
import Rendering.Colour
import Rendering.Gradient
import Rendering.Raster
import Rendering.ArrayRaster
import Rendering.Coord
import qualified Configuration as C
import qualified Types as T
import Prelude hiding (fst, snd)

ifsRoutine :: (Real a, Coefficient a)  => Config a -> Gradient Colour Double -> IO ()
ifsRoutine cfg g = do
    putStrLn ""
    putStrLn "IFS routine."
    putStrLn "Computing scale factors... (experimental)"
    putStrLn $ "Scale factors are: " ++ show (getScales cfg)
    putStrLn "Computing IFS..."
    putStrLn $ "I'm going to write to file '" ++ ifsfile ++ "'."
    getPlot IFS "density" cfg (runWriteImage ifsfile g)
    putStrLn "Done writing to file 'ifs_image.png'. Finished IFS routine."
  where ifsfile = "ifs_image.png"

rootsRoutine :: (Real a, Coefficient a)  => Config a -> Gradient Colour Double -> IO ()
rootsRoutine cfg g = do
    putStrLn ""
    putStrLn "Roots routine."
    putStrLn "Computing roots."
    putStrLn $ "I'm going to write to file '" ++ rootsfile ++ "'."
    getPlot Roots "density" cfg (runWriteImage rootsfile g)
    putStrLn "Done writing to file 'roots_image.png'. Finished roots routine."
  where rootsfile = "roots_image.png"


main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn ""
    handleOptions

runAsCmd :: (Mode, Gradient Colour Double, Config Int) -> IO ()
runAsCmd (mode, g, cfg) = do 
    putStrLn ""
    showConfig cfg
--     showGradient gname g
    putStrLn ""
    case mode of
        Roots -> rootsRoutine cfg g
        IFS -> ifsRoutine cfg g
        Both -> do rootsRoutine cfg g
                   ifsRoutine cfg g
    putStrLn ""
    putStrLn "All done here! Press enter to quit."
    getLine
    putStrLn "Bye!"
--   where g = findGradient gname (fromIntegral $ Types.degree cfg)
--         g' = g ?? opacify black monochrome

runAsGui :: (Mode, Gradient Colour Double, Config Int) -> IO ()
runAsGui (mode, g, cfg) = do
    showConfig cfg
--     showGradient gname g
    putStrLn "    Starting GUI..."
    getPlot mode "source" cfg (runGuiMain (cfgToSettings cfg) g)
--   where g = findGradient gname (fromIntegral $ Types.degree cfg)
--         g' = g ?? opacify black monochrome

showGradient gname Nothing = putStrLn $ concat ["Gradient ", gname, " not found."]
showGradient gname (Just _) = putStrLn $ concat ["Gradient: ", gname]

findGradient name d = opacify black . onInput (* (2 / d)) <$> gradientByName name

runGuiMain s g xs r = do rst <- r
                         runEnvT (guiMain xs rst g) s

runWriteImage fn g xs r = do rst <- r
                             writeImage xs rst g fn


getPlot :: (Real a, Coefficient a) => Mode -> String -> Config a 
        -> (forall f v i o. Foldable f => f i -> IO (IOArrayRaster v i (Maybe Double)) -> r)
        -> r
getPlot Roots "source" cfg k = k (getRoots cfg) $ 
    mapOutput (fmap ((/ 2 ^ fromIntegral (Types.degree cfg)) . fromInteger) . getFirst) <$> 
        mkRasterizer (mkRootPlot sourcePoly) (rbCfg cfg) (ibCfg cfg)
getPlot Roots "density" cfg k = k (getRoots cfg) $ 
    mapOutput (Just . log . (+ 1) . getSum) <$>
        mkRasterizer (mkRootPlot $ density 1) (rbCfg cfg) (ibCfg cfg)
getPlot IFS "density" cfg k = k (ifsPoints cfg) $ 
    mapOutput (Just . log . (+ 1) . getSum) <$> 
        mkRasterizer (mkIFSPlot $ density 1) (rbCfg cfg) (ibCfg cfg)
getPlot _ _ _ _ = error "TODO -- handle getPlot cases"

rbCfg (Config _ (rx,ry) _ _ _) = (mkCd2 0 0, mkCd2 rx ry)
ibCfg (Config _ _ _ c w) = (pair mkCd2 (c - wC), pair mkCd2 (c + wC))
  where wC = (w/2) :+ (w/2)

density :: v -> a -> b -> Sum v
density n _ _ = Sum n

sourcePoly :: (Coefficient cf, Ord cf) => Polynomial cf -> a -> First Integer
sourcePoly p _ = First . Just . round . toAbs
               $ foldr (\x n -> x + 2 * n) 0 (min 1 . max 0 <$> reverse p)

mkRootPlot :: (Polynomial cf -> Root -> v) -> RootPlot cf -> (InpCoord, v)
mkRootPlot f (RootPlot p r) = (pair mkCd2 r, f p r)

mkIFSPlot :: (Polynomial cf -> Root -> v) -> IFSPlot cf -> (InpCoord, v)
mkIFSPlot f (IFSPlot c) = (pair mkCd2 c, f [] c)

askYN :: String -> IO Bool
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
alwaysError _ = return Nothing

-- parseConfigFile :: (Coefficient a) => IO (Maybe (Mode, String, Config a))
-- parseConfigFile = do
--     file' <- handle alwaysError (fmap Just $ openFile "roots.ini" ReadMode)
--     case file' of
--         Nothing -> return Nothing
--         Just file -> do strings <- fmap lines $ hGetContents file
--                         return $ parseMConfig (take 7 strings)

handleOptions :: IO ()
handleOptions = do
    args <- getArgs
    getConfig args
    
--     case args of
--         ("gui":args') -> parseGuiArgs =<< loadConfigFile args'
--         _             -> parseCmdArgs =<< loadConfigFile args
--         _             -> parseCmdArgs =<< parseMConfig args

getConfig [] = loadConfigFile "roots.config"
getConfig (arg:_) = loadConfigFile arg

-- loadConfigFile :: String -> IO (Maybe (Mode, Gradient Colour Double, Config Int))
loadConfigFile fn = do res <- parseConfig fn =<< readFile fn
                       case res of 
                           Left err -> do putStrLn "Error loading config file:"
                                          putStrLn err
--                                           return Nothing
                           Right cfg -> mkConfig cfg



mkConfig c = case get runMode c of WithGUI -> runAsGui cfg
                                   ImageFile -> runAsCmd cfg
  where cfg = configForRender . head $ get renders c

configForRender r = (mode, grad, cfg)
  where (mode, dg) = case get renderMode r of
                         C.Roots d -> (Roots, d)
                         C.IFS d   -> (IFS, d)
        cfg = Config [-1, 1] (toTuple $ get C.outputSize r) dg
                     (coordToComplex $ get renderCenter r)
                     (fst $ get renderSize r)
        grad = onInput (* (2 / fromIntegral dg)) 
             . gradientFromSpec monochrome black $ get gradSpec r
                      

-- parseGuiArgs :: Maybe (Mode, String, Config Int) -> IO ()
-- parseGuiArgs (Just cfg) = putStrLn "Using command line args." >> runAsGui cfg
-- parseGuiArgs Nothing    = do 
--     cfg' <- loadConfigFile
--     case cfg' of
--         Nothing  -> putStrLn "No valid config file found. (roots.ini)"
--         Just cfg -> runAsGui cfg

-- parseCmdArgs :: Maybe (Mode, String, Config Int) -> IO ()
-- parseCmdArgs (Just cfg) = putStrLn "Using command line args." >> runAsCmd cfg
-- parseCmdArgs Nothing    = do 
--     cfg' <- loadConfigFile
--     case cfg' of
--         Nothing -> do 
--             putStrLn "No valid config file found. (roots.ini)"
--             runAsCmd =<< askConfig
--         Just cfg -> do
--             ans <- askYN "Valid config file found. Use config file? (y/n) "
--             if ans then runAsCmd cfg else askAboutEverything


-- askAboutEverything = do useGUI <- askYN "Run as GUI? (y/n)"
--                         if useGUI 
--                             then runAsGui =<< askConfig 
--                             else runAsCmd =<< askConfig 



