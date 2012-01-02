module Plotting where

import Graphics.GD
import Control.Applicative
import Data.Complex
import Types
import Interval
import Image

--------------------------------------------------------------------------------
--Gradients and plotting.

showGradient :: Gradient -> String
showGradient = snd

--Refactoring of gradients more than welcome.

--A black-red-yellow-white gradient, with 0 -> black and h -> white.
warm :: Int -> Gradient
warm h = (warm'.toRGBA, "Warm (height "++show(h)++")")
    where d = ceiling(3*255/fromIntegral h)
          warm' (r,0,0,_) = rgb v1 v2 v3
            where v1 = minimum [255,r+d]
                  v2 = maximum [0,r+d-255]
                  v3 = maximum [0,r+d-511]
          warm' (255,g,0,_) = rgb 255 v1 v2
            where v1 = minimum [255,g+d]
                  v2 = maximum [0,g+d-255]
          warm' (255,255,b,_) = rgb 255 255 v1
            where v1 = minimum [255,b+d]   

flipRB :: Color -> Color
flipRB col = rgb r g b
    where [r,g,b] = (\(a,b,c,d) -> [c,b,a]) $ toRGBA col

--A black-blue-cyan-white gradient, with 0 -> black and h -> white.
cold :: Int -> Gradient
cold h = (cold'.toRGBA, "Cold (height "++show(h)++")")
    where cold' (r,g,b,_) = flipRB $ fst (warm h) (rgb b g r)
          
--Binary "gradient".
binary :: Int -> Gradient
binary _ = (binary'.toRGBA, "Binary")
    where binary' _ = rgb 255 255 255

--A black to white gradient.
grey :: Int -> Gradient
grey h = (grey'.toRGBA, "Grey (height "++show(h)++")")
    where d = ceiling(255/fromIntegral h)
          grey' (a,_,_,_) = rgb b b b
            where b = minimum [255,a+d]

toCoords :: [Root] -> Resolution -> Center -> Width -> [Pixel]
toCoords roots (rx,ry) c w  = map (\z -> ( floor(realPart z), floor(imagPart z)))
                                    $ map (\z -> (rx'/w :+ 0) * (z-p))
                                    $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map (fromIntegral) [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)
