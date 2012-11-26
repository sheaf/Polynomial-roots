{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Overture
import Prelude ()

import qualified Configuration as C (outputSize)
import Configuration.Parsing(runParse, pRunSpec)
import Image (writeImage)
import MainGUI (guiMain)
import Modes
import Pair (Pair(pair), StrictPair)
import Rendering.ArrayRaster (IOArrayRaster)
import Rendering.Coord (Cd2(..), mkCd2)
import Rendering.Raster (Rasterizer(mkRasterizer))
import Settings (get, runEnvT, specToSettings)
import Types

--------------------------------------------------------------------------------
--Routines.

routine :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO()
routine mode cfg col spec = do
    putStrLn "Starting routine."
    putStrLn $ "I'm going to write to file '" ++ file ++ "'."
    getPlot mode cfg col spec (runWriteImage file col)
    putStrLn $ "Done writing to file '" ++ file ++"'. Finished routine."
  where file = "image.png"

runAsCmd :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO ()
runAsCmd mode cfg col spec = do
    routine mode cfg col spec
    putStrLn ""
    putStrLn "All done here. Bye!"

runAsGui :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO ()
runAsGui mode cfg col spec = do
    putStrLn "Starting GUI..."
    getPlot mode cfg col spec (runGuiMain mode spec col)

runGuiMain :: (Foldable f, Mode m) => 
           m -> RunSpec -> ModeColour m 
           -> f i -> IO (IOArrayRaster v i (ColourData (ModeColour m))) -> IO()
runGuiMain mode spec col xs r = do rst <- r
                                   runEnvT (guiMain xs rst col) s
    where s = specToSettings spec

runWriteImage fn g xs r = do rst <- r
                             writeImage xs rst g fn

getrb :: RunSpec -> (Cd2 Int, Cd2 Int)
getrb spec = (mkCd2 0 0, r)
    where r = get (C.outputSize . render) spec

getPlot :: (Mode m) 
        => m -> ModeConfig m -> ModeColour m -> RunSpec
        -> (forall f v i. (Foldable f) 
            => f i -> IO (IOArrayRaster v i (ColourData (ModeColour m))) -> r)
        -> r
getPlot mode cfg col spec k =
    k (getInputData mode cfg) $
    mkRasterizer (\inp -> (pair mkCd2 (toCoord col inp), toData col inp)) (getrb spec) ib
        where c  = get (windowCenter . render) spec
              s  = get (windowSize   . render) spec
              s' = (s*) $ mkCd2 0.5 0.5
              ib = (c - s', c + s')

--------------------------------------------------------------------------------
--Config handling.

mkFromConfig :: AnyConfig -> RunSpec -> IO()
mkFromConfig (AnyConfig mode cfg col) = mkFromConfig' mode cfg col

mkFromConfig' :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO()
mkFromConfig' mode cfg col spec = case get runMode spec of 
                                       WithGUI   -> runAsGui mode cfg col spec
                                       ImageFile -> runAsCmd mode cfg col spec

--------------------------------------------------------------------------------
--Main.

writeError :: String -> IO()
writeError s = do putStrLn "Parse error:"
                  putStrLn s
                  putStrLn "Terminating application."

main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn "Reading configuration from file 'roots.config'."
    let fn = "roots.config"
    spec <- runParse pRunSpec fn =<< readFile fn
    mode <- runParse pAnyMode fn =<< readFile fn
    case (spec, mode) of
         (Left s,_) -> writeError s
         (_,Left s) -> writeError s
         (Right rspec, Right rmode) -> do cfg <- modeConfigFromMode fn rmode
                                          case cfg of
                                               Left s     -> writeError s
                                               Right rcfg -> mkFromConfig rcfg rspec
