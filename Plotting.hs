module Plotting where

import qualified Graphics.GD as GD
import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.RGBSpace.HSV as HSV
import Control.Applicative
import Data.Complex
import Data.Word
import Data.List
import Types
import Interval
import Util

type RGB8 = (Word8, Word8, Word8)
type GDColor = GD.Color

data RootPlot a = RootPlot (Polynomial a) Root 
newtype IFSPlot a = IFSPlot (Complex Double)

type PixelOrig = Integer

type PlotData = (Pixel, PixelOrig)

instance Show (RootPlot a) where show (RootPlot _ x) = show x


