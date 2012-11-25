{-# LANGUAGE TypeFamilies #-}

module IFS where

import Overture
import Prelude ()

import Data.Tree(Forest)

import Interval(Interval((+!)))
import Polynomials
import Roots (canHaveRoots)
import Types

--------------------------------------------------------------------------------
--IFS plotting.

scalePoint :: (Coefficient a) => Complex Double -> Scaler a -- Scaler a ~ (Complex Double -> Polynomial a -> Complex Double)
scalePoint c z p = z * scale
    where scale = (negate . recip . (`evaluate` c)) . derivative . map toComplex $ p

ifsCounts :: (Coefficient a) => ((Polynomial a -> Root) -> (Polynomial a -> b))
          -> Config c a -> Scaler a -> Forest (Polynomial a) -> Forest b
ifsCounts q (Config _ _ _ c _ _ _) f forest = rootForest
    where eval       = flip evaluate c . map toComplex
          rootForest = (map . fmap) (q $ uncurry f . (eval &&& id)) forest

ifsPoints :: (Coefficient a) => ((Polynomial a -> Root) -> (Polynomial a -> b))
          -> Config c a -> Forest b
ifsPoints q cfg@(Config ic (rx,ry) d c w s g) = ifsForest
  where h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        forest = canHaveRoots ic d cI
        ifsForest = ifsCounts q cfg scaling forest
        scaling = case s of
                       Left False -> \z p -> z
                       Left True  -> scalePoint c
                       Right f    -> \z p -> (1-f') * z + f' * scalePoint c z p
                           where f' = toComplex f
