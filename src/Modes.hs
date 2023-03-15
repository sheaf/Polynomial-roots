{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Modes where

-- base
import Control.Arrow
  ( (&&&) )
import Data.Complex
import Data.Foldable
  ( toList )
import Data.Functor.Compose
import Data.Kind
  ( Type )

-- parallel
import Control.Parallel.Strategies
  ( using, parBuffer, rdeepseq )

-- parsec
import Text.Parsec

-- polynomial-roots
import Configuration.Parsing
import IFS
  ( ifsPoints )
import Roots
  ( getRoots )
import Types

--------------------------------------------------------------------------------
--Mode definitions.

class ColourScheme (ModeColour m) => Mode m where
  type ModeColour m :: Type
  type ModeConfig m :: Type
  getInputData :: m -> ModeConfig m -> [InputData (ModeColour m)]
  parseConfig  :: (Monad n) => m -> ParsecT String u n (ModeConfig m)
  extractCol   :: m -> ModeConfig m -> ModeColour m

data IFSDensityMode   a = IFSDensityMode
data IFSSourceMode    a = IFSSourceMode
data RootsSourceMode  a = RootsSourceMode
data RootsDensityMode a = RootsDensityMode

instance (PCoefficient a) => Mode (IFSDensityMode a) where
    type ModeColour (IFSDensityMode a) = DensityCol
    type ModeConfig (IFSDensityMode a) = Config DensityCol a
    getInputData _ = (`using` parBuffer 1000 rdeepseq) . toList
                   . Compose . ifsPoints id
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig pDensityCol

instance (PCoefficient a) => Mode (IFSSourceMode a) where
    type ModeColour (IFSSourceMode a) = SourceCol a
    type ModeConfig (IFSSourceMode a) = Config SourceColB a
    getInputData _ = (`using` parBuffer 1000 rdeepseq) . toList
                   . Compose . ifsPoints (id &&&)
    extractCol   _ = (\ (Config cfs _ _ _ _ _ g) -> addCfs cfs g)
    parseConfig  _ = pModeConfig pSourceCol

instance (PCoefficient a) => Mode (RootsDensityMode a) where
    type ModeColour (RootsDensityMode a) = DensityCol
    type ModeConfig (RootsDensityMode a) = Config DensityCol a
    getInputData _ = concat
                   . (`using` parBuffer 1000 rdeepseq) . toList
                   . Compose . getRoots id
    extractCol   _ = (\ (Config _ _ _ _ _ _ g) -> g)
    parseConfig  _ = pModeConfig pDensityCol

instance (PCoefficient a) => Mode (RootsSourceMode a) where
    type ModeColour (RootsSourceMode a) = SourceCol a
    type ModeConfig (RootsSourceMode a) = Config SourceColB a
    getInputData _ = concat
                   . (`using` parBuffer 1000 rdeepseq) . toList
                   . Compose . getRoots ((uncurry map .) . ( (,) &&& ))
    extractCol   _ = (\ (Config cfs _ _ _ _ _ g) -> addCfs cfs g)
    parseConfig  _ = pModeConfig pSourceCol

--------------------------------------------------------------------------------
-- Some existential types.

data AnyMode   = forall m. (Mode m) => AnyMode m
data AnyConfig = forall m. (Mode m) => AnyConfig m (ModeConfig m) (ModeColour m)

--------------------------------------------------------------------------------
-- Parsing.

pAnyMode :: Monad m => ParsecT String u m AnyMode
pAnyMode = do manyTill anyChar (try $ newline *> pString "mode")
              mode <- pField "" *> ((,,) <$> pStrings ["roots","ifs"]
                                         <*> pStrings ["source", "density"]
                                         <*> pStrings ["int", "double"
                                                      ,"rational", "complex"])
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
                    _                              -> error $ "unknown mode " ++ show mode
              return mode'

modeConfigFromMode :: String -> AnyMode -> IO (Either String AnyConfig)
modeConfigFromMode fn (AnyMode mode) = do
    cfg <- runParse (parseConfig mode) fn =<< readFile fn
    let res = case cfg of
                   Left  s    -> Left s
                   Right conf -> Right (AnyConfig mode conf (extractCol mode conf))
    return res
