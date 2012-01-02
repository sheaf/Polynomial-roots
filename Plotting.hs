{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Plotting where

import qualified Graphics.GD as GD
import Control.Applicative
import Data.Complex
import Data.Word
import Data.List
import Types
import Interval
import Util
-- import Image



type RGB8 = (Word8, Word8, Word8)
type GDColor = GD.Color

class (Ord (Component a), Num (Component a)) => RGB a where
    type Component a :: *
    rgb      :: Component a -> Component a -> Component a -> a
    splitRGB :: a -> (Component a, Component a, Component a)
    red      :: a -> Component a
    green    :: a -> Component a
    blue     :: a -> Component a
    splitRGB c = (red c, green c, blue c)
    red   c = let (r, _, _) = splitRGB c in r
    green c = let (_, g, _) = splitRGB c in g
    blue  c = let (_, _, b) = splitRGB c in b

instance RGB (Int, Int, Int) where
    type Component (Int, Int, Int) = Int
    rgb = (,,)
    splitRGB = id

instance RGB RGB8 where
    type Component RGB8 = Int
    rgb r g b = (clampByte r, clampByte g, clampByte b)
    splitRGB (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

clampByte :: (Integral a, Num b) => a -> b
clampByte x = fromIntegral . min 255 . max 0 $ x

instance RGB GD.Color where
    type Component GD.Color = Int
    rgb r g b = GD.rgb (clampByte r) (clampByte g) (clampByte b)
    splitRGB c = let (r,g,b,_) = GD.toRGBA c in (r,g,b)


--------------------------------------------------------------------------------
-- Gradients 

showGradient :: Gradient v c -> String
showGradient = snd


logIters x = fromInteger . round . (45 *) . log . fromIntegral . getSum $ x

warm :: (RGB c, Integral v) => Int -> Gradient (Sum v) c
warm h = (warm', concat ["Warm (height ", show h, ")"])
  where warm' v = rgb (logIters v) (logIters v - 10*h') (logIters v - 20*h')
        h' = fromIntegral h

cold :: (RGB c, Integral v) => Int -> Gradient (Sum v) c
cold h = (cold', concat ["Cold (height ", show h, ")"])
  where cold' v = rgb (logIters v - 20*h') (logIters v - 10*h') (logIters v) 
        h' = fromIntegral h

binary :: (RGB c, Integral v) => Int -> Gradient (Sum v) c
binary _ = (const $ rgb 255 255 255, "Binary")

stripes :: (RGB c, Integral v) => Int -> Gradient (Sum v) c
stripes h = (stripes', concat ["Stripes (width ", show h, ")"])
  where stripes' v | even (v' `div` h) = rgb 0 0 255
                   | otherwise         = rgb 0 255 0
          where v' = fromIntegral . getSum $ v

grey :: (RGB c, Integral v) => Int -> Gradient (Sum v) c
grey h = (grey', concat ["Grey (height ", show h, ")"])
  where grey' v = let v' = logIters v - 10 * h' in rgb v' v' v'
        h' = fromIntegral h


--------------------------------------------------------------------------------
-- Plotting


data RootPlot a = RootPlot (Polynomial a) Root
newtype IFSPlot a = IFSPlot (Complex Double)

type PixelOrig = Integer

type PlotData = (Pixel, PixelOrig)
