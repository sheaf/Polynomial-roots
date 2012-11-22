{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Overture hiding(fst,snd)
import Prelude ()
import qualified Prelude as P (fst, snd)

import Text.Parsec (ParsecT, try, manyTill, choice, newline, anyChar )
import qualified Text.Parsec as TP (many, optional)

import qualified Configuration as C (outputSize)
import Configuration.Parsing
import IFS (ifsPoints)
import Image (writeImage)
import MainGUI (guiMain)
import Pair (Pair(pair), StrictPair)
import Rendering.ArrayRaster (IOArrayRaster)
import Rendering.Coord (Cd2(..), mkCd2)
import Rendering.Raster (Rasterizer(mkRasterizer))
import Roots (getRoots)
import Settings (get, runEnvT, specToSettings)
import Types

--------------------------------------------------------------------------------
--Mode definitions.

class (ColourScheme (ModeColour m)) => Mode m where
    type ModeColour m :: *
    type ModeConfig m :: *
    getInputData :: m -> ModeConfig m -> [InputData (ModeColour m)]
    parseConfig :: (Monad n) => m -> ParsecT String u n (ModeConfig m)
    extractCol :: m -> ModeConfig m -> ModeColour m

data IFSDensityMode   a = IFSDensityMode
data IFSSourceMode    a = IFSSourceMode
data RootsSourceMode  a = RootsSourceMode
data RootsDensityMode a = RootsDensityMode

instance (PCoefficient a) => Mode (IFSDensityMode a) where
    type ModeColour (IFSDensityMode a) = DensityCol
    type ModeConfig (IFSDensityMode a) = Config DensityCol a
    getInputData _ = (map P.snd) . ifsPoints
    extractCol _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig _ = pModeConfig pDensityCol

instance (PCoefficient a) => Mode (IFSSourceMode a) where
    type ModeColour (IFSSourceMode a) = SourceCol a
    type ModeConfig (IFSSourceMode a) = Config (SourceCol a) a
    getInputData _ = ifsPoints
    extractCol _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig _ = pModeConfig (pSourceCol pCoeff)

instance (PCoefficient a) => Mode (RootsSourceMode a) where
    type ModeColour (RootsSourceMode a) = SourceCol a
    type ModeConfig (RootsSourceMode a) = Config (SourceCol a) a
    getInputData _ = getRoots
    extractCol _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig _ = pModeConfig (pSourceCol pCoeff)

instance (PCoefficient a) => Mode (RootsDensityMode a) where
    type ModeColour (RootsDensityMode a) = DensityCol
    type ModeConfig (RootsDensityMode a) = Config DensityCol a
    getInputData _ = (map P.snd) . getRoots
    extractCol _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig _ = pModeConfig pDensityCol

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
    putStrLn "All done here! Press enter to quit."
    getLine
    putStrLn "Bye!"

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

getPlot :: (Mode m) => m -> ModeConfig m -> ModeColour m -> RunSpec
                    -> (forall f v i. Foldable f => f i -> IO (IOArrayRaster v i (ColourData (ModeColour m))) -> r)
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

mkFromConfig :: Mode m => m -> ModeConfig m -> ModeColour m -> RunSpec -> IO()
mkFromConfig mode cfg col spec = case get runMode spec of 
                                      WithGUI   -> runAsGui mode cfg col spec
                                      ImageFile -> runAsCmd mode cfg col spec

--------------------------------------------------------------------------------
--Routines for existential types.

data AnyMode = forall m. (Mode m) => AnyMode m
data AnyConfig = forall m. (Mode m) => AnyConfig m (ModeConfig m) (ModeColour m)
data AnyPCoeff = forall a. (PCoefficient a) => PCoeff a

mkFromConfig' :: AnyConfig -> RunSpec -> IO()
mkFromConfig' (AnyConfig mode cfg col) = mkFromConfig mode cfg col

--------------------------------------------------------------------------------
--Parsing.

pAnyMode :: Monad m => ParsecT String u m AnyMode
pAnyMode = do manyTill anyChar (try $ newline *> pString "mode")
              mode <- pField "" *> ((,,) <$> choice [pString "roots", pString "ifs"] 
                                         <*> choice [pString "source", pString "density"]
                                         <*> choice [pString "int", pString "double", pString "complex", pString "rational"])
              TP.optional pFieldSep
              TP.many newline
              pString "{"
              let mode' = case mode of
              --sorry!!
                               ("roots","source" ,"int"     ) -> AnyMode (RootsSourceMode  :: RootsSourceMode  Int             )
                               ("roots","density","int"     ) -> AnyMode (RootsDensityMode :: RootsDensityMode Int             )
                               ("ifs"  ,"source" ,"int"     ) -> AnyMode (IFSSourceMode    :: IFSSourceMode    Int             )
                               ("ifs"  ,"density","int"     ) -> AnyMode (IFSDensityMode   :: IFSDensityMode   Int             )
                               ("roots","source" ,"double"  ) -> AnyMode (RootsSourceMode  :: RootsSourceMode  Double          )
                               ("roots","density","double"  ) -> AnyMode (RootsDensityMode :: RootsDensityMode Double          )
                               ("ifs"  ,"source" ,"double"  ) -> AnyMode (IFSSourceMode    :: IFSSourceMode    Double          )
                               ("ifs"  ,"density","double"  ) -> AnyMode (IFSDensityMode   :: IFSDensityMode   Double          )
                               ("roots","source" ,"rational") -> AnyMode (RootsSourceMode  :: RootsSourceMode  Rational        )
                               ("roots","density","rational") -> AnyMode (RootsDensityMode :: RootsDensityMode Rational        )
                               ("ifs"  ,"source" ,"rational") -> AnyMode (IFSSourceMode    :: IFSSourceMode    Rational        )
                               ("ifs"  ,"density","rational") -> AnyMode (IFSDensityMode   :: IFSDensityMode   Rational        )
                               ("roots","source" ,"complex" ) -> AnyMode (RootsSourceMode  :: RootsSourceMode  (Complex Double))
                               ("roots","density","complex" ) -> AnyMode (RootsDensityMode :: RootsDensityMode (Complex Double))
                               ("ifs"  ,"source" ,"complex" ) -> AnyMode (IFSSourceMode    :: IFSSourceMode    (Complex Double))
                               ("ifs"  ,"density","complex" ) -> AnyMode (IFSDensityMode   :: IFSDensityMode   (Complex Double))
              return mode'

modeConfigFromMode :: String -> AnyMode -> IO(Either String AnyConfig)
modeConfigFromMode fn (AnyMode mode) = do
    cfg <- runParse (parseConfig mode) fn =<< readFile fn
    let res = case cfg of
                   Left s    -> Left s
                   Right cfg -> Right (AnyConfig mode cfg (extractCol mode cfg))
    return res

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
                                               Right rCfg -> mkFromConfig' rCfg rspec
