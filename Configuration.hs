{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration where

import AltPrelude
import Prelude ()
import Data.Label (mkLabels)
import Util
import Pair
import Rendering.Coord hiding (size)

type Degree = Int

data RenderMode = Roots Degree 
                | IFS Degree
    deriving (Eq, Ord, Read, Show)

data RenderSpec = RenderSpec { _mode   :: RenderMode
                             , _center :: Cd2 Double
                             , _size   :: Cd2 Double
                             } deriving (Eq, Ord, Read, Show)

data AspectCorrection = None | Clip | Expand
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data BlendFunction = Blend | Overlay
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data TransFunc = Invert | Exponent Double | Reverse
    deriving (Eq, Ord, Read, Show)


data GradientSpec = NamedGradient String
                   | Split [(GradientSpec, Double)] GradientSpec
                   | Combine BlendFunction [GradientSpec]
                   | Transform TransFunc GradientSpec
    deriving (Eq, Ord, Read, Show)

data Render m = Render { _renderSpec :: RenderSpec
                     , _outputFile :: Maybe String
                     , _outputSize :: Cd2 Int
                     , _fixAspect  :: AspectCorrection
                     , _gradSpec   :: GradientSpec
                     } deriving (Eq, Ord, Read, Show)

data RunMode = ImageFile | WithGUI 
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Configuration m = Cfg { _runMode :: RunMode
                           , _renders :: [Render m]
                           } deriving (Eq, Ord, Read, Show)

$(mkLabels [''RenderSpec, ''Render, ''Configuration])

renderMode = mode . renderSpec
renderCenter = center . renderSpec
renderSize = size . renderSpec
