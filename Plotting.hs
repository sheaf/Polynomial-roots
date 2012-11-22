{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Plotting where

import Overture
import Prelude ()
import Control.Applicative

import Interval (elemI)
import Types (Pixel)

type PixelOrig = Integer

type PlotData = (Pixel, PixelOrig)

toCoords roots (rx,ry) c w  = map((\z->(floor(realPart z),ry-floor(imagPart z)))
                                  .(\z -> (rx'/w :+ 0) * (z-p)))
                              $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map fromIntegral [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)
