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
import Image



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
    rgb r g b = (clampWord8 r, clampWord8 g, clampWord8 b)
    splitRGB (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

clampWord8 = fromIntegral . min 255 . max 0

instance RGB GD.Color where
    type Component GD.Color = Int
    rgb = GD.rgb
    splitRGB c = let (r,g,b,_) = GD.toRGBA c in (r,g,b)


--------------------------------------------------------------------------------
-- Gradients 

showGradient :: Gradient c -> String
showGradient = snd

--Refactoring of gradients more than welcome.

--A black-red-yellow-white gradient, with 0 -> black and h -> white.
-- warm :: Int -> Gradient
warm :: (RGB c) => Int -> Gradient c
warm h = (warm'.splitRGB, "Warm (height "++show(h)++")")
    where d = fromInteger $ ceiling(3*255/fromIntegral h)
          warm' (r,0,0) = rgb v1 v2 v3
            where v1 = minimum [255,r+d]
                  v2 = maximum [0,r+d-255]
                  v3 = maximum [0,r+d-511]
          warm' (255,g,0) = rgb 255 v1 v2
            where v1 = minimum [255,g+d]
                  v2 = maximum [0,g+d-255]
          warm' (255,255,b) = rgb 255 255 v1
            where v1 = minimum [255,b+d]   

flipRB :: (RGB c) => c -> c
flipRB col = rgb r g b
    where [r,g,b] = (\(a,b,c) -> [c,b,a]) $ splitRGB col

--A black-blue-cyan-white gradient, with 0 -> black and h -> white.
cold :: (RGB c) => Int -> Gradient c
cold h = (cold'.splitRGB , "Cold (height "++show(h)++")")
    where cold' (r,g,b) = flipRB $ fst (warm h) (rgb b g r)
          
--Binary "gradient".
binary :: (RGB c) => Int -> Gradient c
binary _ = (binary'.splitRGB , "Binary")
    where binary' _ = rgb 255 255 255

--A black to white gradient.
grey :: (RGB c) => Int -> Gradient c
grey h = (grey'.splitRGB , "Grey (height "++show(h)++")")
    where d = fromInteger $ ceiling(255/fromIntegral h)
          grey' (a,_,_) = rgb b b b
            where b = minimum [255,a+d]

--------------------------------------------------------------------------------
-- Plotting

-- plotPixels :: (Plot a, CoeffType a ~ b, Coefficient b) 
--            => Config c b -> [a] -> [Pixel]
-- plotPixels cfg xs = toPixels cfg xs


data RootPlot a = RootPlot (Polynomial a) Root
newtype IFSPlot a = IFSPlot (Complex Double)

type PixelOrig = Integer

type PlotData = (Pixel, PixelOrig)

class Plot a where
    type CoeffType a :: *
    toPixels   :: Config c (CoeffType a) -> [a] -> [Pixel]
    toOrig     :: Config c (CoeffType a) -> [a] -> [PixelOrig]
    plotPixels :: Config c (CoeffType a) -> [a] -> [PlotData]
    toPixels cfg xs = fst <$> plotPixels cfg xs
    toOrig cfg xs = snd <$> plotPixels cfg xs

instance (Real a, Coefficient a) => Plot (RootPlot a) where
    type CoeffType (RootPlot a) = a
    plotPixels (Config ic res d c w g) rs = zip roots origs
      where getPlot (RootPlot _ root) = root
            getOrig (RootPlot orig _) = encodeCoeffs orig
            roots = toCoords (map getPlot rs) res c w
            origs = map getOrig rs
            encodeCoeffs = foldl' (\x y -> x * 2 + if y <= 0 then 0 else 1) 0

instance Plot (IFSPlot a) where
    type CoeffType (IFSPlot a) = a
    plotPixels (Config _ res _ _ w _) pts = zip points (repeat 1)
      where getPlot (IFSPlot p) = p
            points = toCoords (getPlot <$> pts) res (0:+0) w

toCoords :: [Root] -> Resolution -> Center -> Width -> [Pixel]
toCoords roots (rx,ry) c w  = map((\z->(floor(realPart z),ry-floor(imagPart z)))
                                  .(\z -> (rx'/w :+ 0) * (z-p)))
                              $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map fromIntegral [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)


