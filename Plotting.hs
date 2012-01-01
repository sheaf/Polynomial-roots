module Plotting where

import Data.Complex
import Types
import Interval

--------------------------------------------------------------------------------
--Gradients and plotting routines.

showGradient :: Gradient -> String
showGradient = snd

--A black-red-yellow-orange-white gradient, with 0 -> black and h -> white.
warm :: Int -> Gradient
warm h = ((map floor).((map (255*)).warm'), "Warm (height "++show(h)++")")
    where warm' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | (n > 0 && n' <= h'/3) = [3*n'/h',0,0]
            | (n' > h'/3 && n' <= 2*h'/3) = [1, 3*n'/h'-1,0]
            | (n'> 2*h'/3 && n < h) = [1,1,3*n'/h'-2]
                where [h',n'] = map fromIntegral [h,n]

--A black-blue-cyan-white gradient.
cold :: Int -> Gradient
cold h = ((map floor).((map (255*)).cold'), "Cold (height "++show(h)++")")
    where cold' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | (n > 0 && n' <= h'/3) = [0,0,3*n'/h']
            | (n' > h'/3 && n' <= 2*h'/3) = [0, 3*n'/h'-1,1]
            | (n' > 2*h'/3 && n < h) = [3*n'/h'-2,1,1]
                where [h',n'] = map fromIntegral [h,n]

--Black to white gradient.
grey h = ((map floor).((map (255*)).grey'), "Cold (height "++show(h)++")")
    where grey' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | otherwise = [n'/h',n'/h',n'/h']
                where [h',n'] = map fromIntegral [h,n]

--Binary black or white gradient.
binary h = ((map floor).((map (255*)).binary'), "Cold (height "++show(h)++")")
    where binary' n 
            | n < h = [0,0,0]
            | otherwise = [1,1,1]

toCoords :: [Root] -> Resolution -> Center -> Width -> [Pixel]
toCoords roots (rx,ry) c w  = map (\z -> ( floor(realPart z), floor(imagPart z)))
                                    $ map (\z -> (rx'/w :+ 0) * (z-p))
                                    $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map (fromIntegral) [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)
