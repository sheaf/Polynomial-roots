{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration where

import Overture
import Prelude ()
import Data.Label (mkLabels)

import Rendering.Coord (Cd2)
import Rendering.Colour (AlphaColour)

data WindowSpec = WindowSpec { _center :: Cd2 Double
                             , _size   :: Cd2 Double
                             } deriving (Eq, Ord, Read, Show)

data AspectCorrection = None | Clip | Expand
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data BlendFunction = Blend | Overlay
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data GradientSpec = NamedGradient (String, Maybe Double)
                  | Split [(GradientSpec, Double)]
                  | Combine BlendFunction [GradientSpec]
                  | Collate [(AlphaColour Double, Double)]
    deriving (Eq, Read, Show)

data RenderSpec = RenderSpec { _windowSpec :: WindowSpec
                             , _outputFile :: Maybe String
                             , _outputSize :: Cd2 Int
                             , _fixAspect  :: AspectCorrection
                             } deriving (Eq, Read, Show)

data RunMode = ImageFile | WithGUI 
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data RunSpec = RunSpec { _runMode :: RunMode
                       , _renders :: [RenderSpec]
                       } deriving (Eq, Read, Show)

$(mkLabels [''WindowSpec, ''RenderSpec, ''RunSpec])

windowCenter = center . windowSpec
windowSize = size . windowSpec
