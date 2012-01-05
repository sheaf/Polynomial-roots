module Plotting where

import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.RGBSpace.HSV as HSV
import Control.Applicative
import Data.Complex
import Data.Word
import Data.List
import Types
import Interval
import Util

data RootPlot a = RootPlot (Polynomial a) Root 
newtype IFSPlot a = IFSPlot (Complex Double)

type PixelOrig = Integer

type PlotData = (Pixel, PixelOrig)

instance Show (RootPlot a) where show (RootPlot _ x) = show x
toCoords roots (rx,ry) c w  = map((\z->(floor(realPart z),ry-floor(imagPart z)))
                                  .(\z -> (rx'/w :+ 0) * (z-p)))
                              $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map fromIntegral [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)


