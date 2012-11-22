{-# LANGUAGE TypeFamilies #-}

module IFS where

import Overture
import Prelude ()

import Interval(Interval((+!)))
import Polynomials
import Roots (canHaveRoots)
import Types

--------------------------------------------------------------------------------
--IFS plotting.

scalePoint :: (Coefficient a) => Complex Double -> Scaler a -- Scaler a ~ (Complex Double -> Polynomial a -> Complex Double)
scalePoint c z p = z * scale
    where scale = (negate . recip . (`evaluate` c)) . derivative . map toComplex $ p

ifsCounts :: (Coefficient a) => Config c a -> Scaler a -> [Polynomial a] -> [(Polynomial a, Root)]
ifsCounts (Config _ _ _ c _ _ _) f ps = points
    where points = map (\p -> (p,f (flip evaluate c . map toComplex $ p) p)) ps

ifsPoints :: (Coefficient a) => Config c a -> [(Polynomial a, Root)]
ifsPoints cfg@(Config ic (rx,ry) d c w s g) = ifspoints
  where h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        pols = canHaveRoots ic d cI
        ifspoints = ifsCounts cfg scaling pols
        scaling = case s of
                       Left False -> \z p -> z
                       Left True  -> scalePoint c
                       Right f    -> \z p -> (1-f') * z + f' * scalePoint c z p
                           where f' = toComplex f
