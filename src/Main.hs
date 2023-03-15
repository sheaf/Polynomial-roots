module Main where

-- base
import System.Environment
  ( getArgs )

-- polynomial-roots
import Configuration
  hiding ( outputSize )
import qualified Configuration as WindowSpec
  ( WindowSpec(..) )
import qualified Configuration as C
  ( outputSize )
import Configuration.Parsing
  ( runParse, pRunSpec )
import Image
  ( writeImage )
import MainGUI
  ( guiMain )
import Modes
import Pair
  ( Pair(pair) )
import Rendering.ArrayRaster
  ( IOArrayRaster )
import Rendering.Coord
  ( Cd2(..), mkCd2 )
import Rendering.Raster
  ( Rasterizer(mkRasterizer) )
import Settings
  ( runEnvT, specToSettings )
import Types
import Util

--------------------------------------------------------------------------------
--Routines.

routine :: Mode m
        => m -> ModeConfig m -> ModeColour m -> RunSpec -> String -> IO()
routine mode cfg col spec file = do
    putStrLn "Starting routine."
    putStrLn $ "I'm going to write to file '" ++ file ++ "'."
    getPlot mode cfg col spec (runWriteImage file col)
    putStrLn $ "Done writing to file '" ++ file ++"'. Finished routine."

runAsCmd :: Mode m
         => m -> ModeConfig m -> ModeColour m -> RunSpec -> String -> IO()
runAsCmd mode cfg col spec file = do
    routine mode cfg col spec file
    putStrLn ""
    putStrLn "All done here. Bye!"

runAsGui :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO ()
runAsGui mode cfg col spec = do
    putStrLn "Starting GUI..."
    getPlot mode cfg col spec (runGuiMain mode spec col)

runGuiMain :: (Foldable f, Mode m) =>
           m -> RunSpec -> ModeColour m
           -> f i -> IO (IOArrayRaster v i (ColourData (ModeColour m))) -> IO()
runGuiMain _mode spec col xs r =
  do rst <- r
     runEnvT (guiMain xs rst col) (specToSettings spec)

runWriteImage :: (Foldable f, ColourScheme c)
              => FilePath -> c -> f i
              -> IO (IOArrayRaster v i (ColourData c))
              -> IO ()
runWriteImage fn g xs r =
  do rst <- r
     writeImage xs rst g fn

getrb :: RunSpec -> (Cd2 Int, Cd2 Int)
getrb spec = (mkCd2 0 0, r)
    where r = (C.outputSize . render) spec

getPlot :: (Mode m)
        => m -> ModeConfig m -> ModeColour m -> RunSpec
        -> (forall f v i. (Foldable f)
            => f i -> IO (IOArrayRaster v i (ColourData (ModeColour m))) -> r)
        -> r
getPlot mode cfg col spec k =
    k (getInputData mode cfg) $
    mkRasterizer (\inp -> (pair mkCd2 (toCoord col inp), toData col inp))
                 (getrb spec)
                 ib
        where c  = ( WindowSpec.center . windowSpec . render ) spec
              s  = ( WindowSpec.size   . windowSpec . render ) spec
              s' = (s*) $ mkCd2 0.5 0.5
              ib = (c - s', c + s')

--------------------------------------------------------------------------------
--Config handling.

mkFromConfig :: AnyConfig -> RunSpec -> String -> IO()
mkFromConfig (AnyConfig mode cfg col) = mkFromConfig' mode cfg col

mkFromConfig' :: Mode m
              => m -> ModeConfig m -> ModeColour m -> RunSpec -> String -> IO()
mkFromConfig' mode cfg col spec file =
    case runMode spec of
         WithGUI   -> runAsGui mode cfg col spec
         ImageFile -> runAsCmd mode cfg col spec file

--------------------------------------------------------------------------------
--Main.

main :: IO()
main = do
    args <- getArgs
    def_imagefn <- nextImageName (\n -> case n of
                                             1 -> "image.png"
                                             _ -> "image" ++ show n ++ ".png")
    let imagefn = case filter (endsWith ".png") args of
                       (x:_) -> x
                       _     -> def_imagefn
    let cfgfn   = case filter (endsWith ".config") args of
                       (x:_) -> x
                       _     -> "roots.config"
    putStrLn "This program produces images of polynomial roots."
    putStrLn $ "Reading configuration from file '" ++ cfgfn ++ "'."
    spec <- runParse pRunSpec cfgfn =<< readFile cfgfn
    mode <- runParse pAnyMode cfgfn =<< readFile cfgfn
    case (spec, mode) of
      (Left s,_) -> printParseError s
      (_,Left s) -> printParseError s
      (Right rspec, Right rmode) ->
        do cfg <- modeConfigFromMode cfgfn rmode
           case cfg of
                Left s     -> printParseError s
                Right rcfg -> mkFromConfig rcfg rspec imagefn
