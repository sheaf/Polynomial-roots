{- Polynomial roots.
Computes the set of polynomials with a particular restricted set of coeffs,
who might have roots in a particular rectangle in the complex plane,
using tree pruning.-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where

import Overture hiding(fst,snd)
import Prelude ()
import Control.Exception(IOException,handle)
import System.Environment(getArgs)
import System.IO
import Data.Char
import Data.Foldable (Foldable, toList)
import Data.Maybe
import Data.Monoid
import Roots
import IFS
import Types
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

ifsRoutine :: (Real a, Coefficient a, Monoid m, m ~ Sum Double) => Config m a -> IO ()
ifsRoutine cfg = do
    let g = cgradient cfg
    putStrLn ""
    putStrLn "IFS routine."
    putStrLn "Computing scale factors... (experimental)"
    putStrLn $ "Scale factors are: " ++ show (getScales cfg)
    putStrLn "Computing IFS..."
    putStrLn $ "I'm going to write to file '" ++ ifsfile ++ "'."
    getPlot IFS cfg (runWriteImage ifsfile g)
    putStrLn "Done writing to file 'ifs_image.png'. Finished IFS routine."
  where ifsfile = "ifs_image.png"

rootsRoutine :: (Real a, Coefficient a, Monoid m, m ~ Sum Double) => Config m a -> IO ()
rootsRoutine cfg = do
    let g = cgradient cfg
    putStrLn ""
    putStrLn "Roots routine."
    putStrLn "Computing roots."
    putStrLn $ "I'm going to write to file '" ++ rootsfile ++ "'."
    getPlot Roots cfg (runWriteImage rootsfile g)
    putStrLn "Done writing to file 'roots_image.png'. Finished roots routine."
  where rootsfile = "roots_image.png"

runAsCmd :: (Monoid m, m ~ Sum Double) => (Mode, Config m Int) -> IO ()
runAsCmd (mode, cfg) = do 
    putStrLn ""
    showConfig cfg
    putStrLn ""
    case mode of
        Roots -> rootsRoutine cfg
        IFS -> ifsRoutine cfg
        Both -> do rootsRoutine cfg
                   ifsRoutine cfg
    putStrLn ""
    putStrLn "All done here! Press enter to quit."
    getLine
    putStrLn "Bye!"

runAsGui :: (Monoid m, m ~ Sum Double) => (Mode, Config m Int) -> IO ()
runAsGui (mode, cfg) = do
    putStrLn ""
    showConfig cfg
    putStrLn "    Starting GUI..."
    getPlot mode cfg (runGuiMain (cfgToSettings cfg) (cgradient cfg))

runGuiMain s g xs r = do rst <- r
                         runEnvT (guiMain xs rst g) s

runWriteImage fn g xs r = do rst <- r
                             writeImage xs rst g fn

--TODO: allow other monoids than m ~ Sum Double
getPlot :: (Real a, Coefficient a, Monoid m, m ~ Sum Double) => Mode -> Config m a 
        -> (forall f v i. Foldable f => f i -> IO (IOArrayRaster v i m) -> r)
        -> r
--getPlot Roots cfg k = -- (k :: [RootPlot a] -> IO (IOArrayRaster SourceSum (RootPlot a) SourceSum) -> r) = 
--    k (getRoots cfg) $ 
--    mkRasterizer (mkRootPlot $ source1 [1,-1]) (rbCfg cfg) (ibCfg cfg)
getPlot Roots cfg k = --(k :: [RootPlot a] -> IO (IOArrayRaster (Sum Double) (RootPlot a) (Sum Double)) -> r) = 
    k (getRoots cfg) $
    mkRasterizer (mkRootPlot density) (rbCfg cfg) (ibCfg cfg)
getPlot IFS cfg k = --(k :: [IFSPlot a] -> IO (IOArrayRaster (Sum Double) (IFSPlot a) (Sum Double)) -> r) = 
    k (ifsPoints cfg) $ 
    mkRasterizer (mkIFSPlot density) (rbCfg cfg) (ibCfg' cfg)
getPlot _ _ _ = error "TODO -- handle getPlot cases"

rbCfg (Config _ (rx,ry) _ _ _ _) = (mkCd2 0 0, mkCd2 rx ry)
ibCfg (Config _ _ _ c w _) = (pair mkCd2 (c - wC), pair mkCd2 (c + wC))
    where wC = (w/2) :+ (w/2)
ibCfg' (Config _ _ _ c w _) = (pair mkCd2 (0 - wC), pair mkCd2 (0 + wC))
    where wC = (w/2) :+ (w/2)

mkRootPlot :: (Polynomial cf -> Root -> v) -> RootPlot cf -> (InpCoord, v)
mkRootPlot f (RootPlot p r) = (pair mkCd2 r, f p r)

mkIFSPlot :: (Polynomial cf -> Root -> v) -> IFSPlot cf -> (InpCoord, v)
mkIFSPlot f (IFSPlot c) = (pair mkCd2 c, f [] c)

handleOptions :: Monoid m => IO(Configuration m c)
handleOptions = do
    args <- getArgs
    getConfig args

getConfig :: Monoid m => [String] -> IO(Configuration m c)
--getConfig [] = loadConfigFile "roots.config"
--getConfig (arg:_) = loadConfigFile arg
getConfig _ = loadConfigFile "roots.config"

loadConfigFile :: Monoid m => String -> IO(Configuration m c)
loadConfigFile fn = do res <- parseConfig fn =<< readFile fn
                       case res of 
                           Left err -> do putStrLn "Error loading config file:"
                                          error err
                           Right cfg -> return cfg

mkConfig :: (c ~ RGBAColour, Monoid m, m ~ Sum Double) => Configuration m c -> IO() 
mkConfig c = case get runMode c of WithGUI -> runAsGui cfg
                                   ImageFile -> runAsCmd cfg
  where cfg = configForRender rdr
        rdr = case head $ get renders c of
                   Just rdr' -> rdr'
                   Nothing   -> error "empty list of renders..."

--TODO: make this return different gradients (using different monoids).
configForRender :: (c ~ RGBAColour, Monoid m, m ~ Sum Double) => Render m c -> (Mode, Config m Int)
configForRender r = (mode, cfg)
  where (mode, dg) = case get renderMode r of
                         C.Roots d -> (Roots, d)
                         C.IFS d   -> (IFS, d)
        cfg = Config [-1, 1] (toTuple $ get C.outputSize r) dg
                     (coordToComplex $ get renderCenter r)
                     (fst $ get renderSize r)
                     g 
        g = gradientFromSpec monochrome black (get gradSpec r) --testing!!
                      
main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn ""
    cfg <- handleOptions
    mkConfig cfg
