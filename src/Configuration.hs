module Configuration where

-- polynomial-roots
import Rendering.Colour
  ( AlphaColour )
import Rendering.Coord
  ( Cd2 )

--------------------------------------------------------------------------------

data WindowSpec
  = WindowSpec
  { center :: Cd2 Double
  , size   :: Cd2 Double
  }
  deriving stock ( Eq, Ord, Read, Show )

data AspectCorrection = None | Clip | Expand
  deriving stock ( Eq, Ord, Read, Show, Enum, Bounded )

data BlendFunction = Blend | Overlay
  deriving stock ( Eq, Ord, Read, Show, Enum, Bounded )

data GradientSpec
  = NamedGradient (String, Maybe Double)
  | Split [(GradientSpec, Double)]
  | Combine BlendFunction [GradientSpec]
  | Collate [(AlphaColour Double, Double)]
  deriving stock ( Eq, Read, Show )

data RenderSpec
  = RenderSpec
  { windowSpec :: WindowSpec
  , outputFile :: Maybe String
  , outputSize :: Cd2 Int
  , fixAspect  :: AspectCorrection
  }
  deriving stock ( Eq, Read, Show )

data RunMode = ImageFile | WithGUI
  deriving stock ( Eq, Ord, Read, Show, Enum, Bounded )

data RunSpec
  = RunSpec
  { runMode :: RunMode
  , render  :: RenderSpec
  }
  deriving stock ( Eq, Read, Show )
