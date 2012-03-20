{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration where

import Overture
import Prelude ()
import Data.Label (mkLabels)
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

type TransFunc m n = m -> n

data GradientSpec = NamedGradient String -- | Source
    deriving (Eq, Ord, Read, Show)

data Render = Render { _renderSpec :: RenderSpec
                     , _outputFile :: Maybe String
                     , _outputSize :: Cd2 Int
                     , _fixAspect  :: AspectCorrection
                     , _gradSpec   :: GradientSpec
                     } deriving (Eq, Ord, Read, Show)

data RunMode = ImageFile | WithGUI 
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Configuration = Cfg { _runMode :: RunMode
                         , _renders :: [Render]
                         } deriving (Eq, Ord, Read, Show)

$(mkLabels [''RenderSpec, ''Render, ''Configuration])

renderMode = mode . renderSpec
renderCenter = center . renderSpec
renderSize = size . renderSpec
