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

toifs :: (Coefficient a) => IterCoeffs a -> Point -> IFS
toifs coeffs c = (toifs' coeffs c, [c])
    where toifs' coeffs' c' z = map ((\cf -> cf*z*c' + 1) . toComplex) coeffs'
        --two normalisations, z -> (cf z c + 1) and z -> (zc + cf)

scalePoint :: (Coefficient a) => Complex Double -> Scaler a -- Scaler a ~ (Complex Double -> Polynomial a -> Complex Double)
scalePoint c z p = z * scale
    where scale = (negate . recip . (`evaluate` c)) . derivative . map toComplex $ p

ifsCheatCounts :: (Coefficient a) => Config c a -> Scaler a -> [Polynomial a] -> [(Polynomial a, Root)]
ifsCheatCounts (Config _ _ _ c _ _ _) f ps = points
    where points = map (\p -> (p,f (flip evaluate c . map toComplex $ p) p)) ps

ifsIterates :: Iterations -> IFS -> [Complex Double]
ifsIterates 0 (_,vals) = vals
ifsIterates n (fs,vals) = fs =<< ifsIterates (n-1) (fs,vals)

ifsCounts :: (Coefficient a) => [Complex Double] -> IFS 
          -> Config c a -> [Complex Double]
ifsCounts scales ifs (Config _ _ d _ _ _ _) = points
    where points' = ifsIterates d ifs
          points = case scales of
                        [] -> points'
                        _ -> (\x -> map (x*) scales) =<< points'

ifsPoints :: (Coefficient a) => Config c a -> [(Polynomial a, Root)]
ifsPoints cfg@(Config ic (rx,ry) d c w s g) = ifspoints
  where --ifs = toifs ic c
        --ifspoints = ifsCounts [] ifs cfg
        h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        --cI = ((-8):+(-8),8:+8) --no cheating, don't do any pruning!
        pols = canHaveRoots ic d cI
        ifspoints = ifsCheatCounts cfg scaling pols
        scaling = case s of
                       Left False -> \z p -> z
                       Left True  -> scalePoint c
                       Right f    -> \z p -> (1-f') * z + f' * scalePoint c z p
                           where f' = toComplex f
