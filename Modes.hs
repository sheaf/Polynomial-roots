{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Modes where

import Control.Applicative((<$>),(<*>),(*>))
import Data.Functor.Compose
import Data.Traversable
import Data.Tree (Tree)
import Text.Parsec

import IFS (ifsPoints)
import Configuration.Parsing
import Roots (getRoots)
import Types

--------------------------------------------------------------------------------
--Mode definitions.

class (ColourScheme (ModeColour m), Traversable (Traversor m)) => Mode m where
    type ModeColour m :: *
    type ModeConfig m :: *
    type Traversor  m :: * -> *
    getInputData :: m -> ModeConfig m -> Traversor m (InputData (ModeColour m))
    parseConfig  :: (Monad n) => m -> ParsecT String u n (ModeConfig m)
    extractCol   :: m -> ModeConfig m -> ModeColour m

data IFSDensityMode   a = IFSDensityMode
data IFSSourceMode    a = IFSSourceMode
data RootsSourceMode  a = RootsSourceMode
data RootsDensityMode a = RootsDensityMode

instance (PCoefficient a) => Mode (IFSDensityMode a) where
    type ModeColour (IFSDensityMode a) = DensityCol
    type ModeConfig (IFSDensityMode a) = Config DensityCol a
    type Traversor  (IFSDensityMode a) = Compose [] Tree
    getInputData _ = (fmap snd) . Compose . ifsPoints
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig pDensityCol

instance (PCoefficient a) => Mode (IFSSourceMode a) where
    type ModeColour (IFSSourceMode a) = SourceCol a
    type ModeConfig (IFSSourceMode a) = Config (SourceCol a) a
    type Traversor  (IFSSourceMode a) = Compose [] Tree
    getInputData _ = Compose . ifsPoints
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig (pSourceCol pCoeff)

instance (PCoefficient a) => Mode (RootsSourceMode a) where
    type ModeColour (RootsSourceMode a) = SourceCol a
    type ModeConfig (RootsSourceMode a) = Config (SourceCol a) a
    type Traversor  (RootsSourceMode a) = Compose [] (Compose Tree [])
    getInputData _ = Compose . fmap Compose 
                   . map (fmap (\(p,rs) -> map (p,) rs)) . getRoots
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig (pSourceCol pCoeff)

instance (PCoefficient a) => Mode (RootsDensityMode a) where
    type ModeColour (RootsDensityMode a) = DensityCol
    type ModeConfig (RootsDensityMode a) = Config DensityCol a
    type Traversor  (RootsDensityMode a) = Compose [] (Compose Tree [])
    getInputData _ = Compose . fmap Compose 
                   . map (fmap snd) . getRoots
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig pDensityCol

--------------------------------------------------------------------------------
--Some existential types.

data AnyMode = forall m. (Mode m) => AnyMode m
data AnyConfig = forall m. (Mode m) => AnyConfig m (ModeConfig m) (ModeColour m)
data AnyPCoeff = forall a. (PCoefficient a) => PCoeff a

--------------------------------------------------------------------------------
--Parsing.

pAnyMode :: Monad m => ParsecT String u m AnyMode
pAnyMode = do manyTill anyChar (try $ newline *> pString "mode")
              mode <- pField "" *> ((,,) <$> choice [pString "roots", pString "ifs"] 
                                         <*> choice [pString "source", pString "density"]
                                         <*> choice [pString "int", pString "double", pString "complex", pString "rational"])
              optional pFieldSep
              many newline
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
