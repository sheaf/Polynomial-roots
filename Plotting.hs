{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Plotting where

import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.RGBSpace.HSV as HSV
import Control.Applicative
import Data.Complex
import Data.List
import Data.Monoid
import Data.Word
import Rendering.Gradient
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

--------------------------------------------------------------------------------
--Defines different monoids amenable to colourings.

--Density colouring.                                  
density :: p -> r -> Sum Double
density _ _ = Sum 0.1

--Think: (RGB, Opacity).
newtype SourceSum = Source (Double, Double)
instance Monoid SourceSum where
    mempty = Source (0,0) -- technically any (n,0)
    mappend (Source (x,n)) (Source (y,m)) = Source ( (x*n + y*m)/(n+m), n+m-n*m) 

--Colouring by source polynomial, "base n" and "scale factor" methods.
source1, source2 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Complex Double -> SourceSum
source1 cfs p _ = Source (toGValue1 cfs p,1)
source2 cfs p r = Source (toGValue2 cfs p r,1)
