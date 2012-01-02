module IFS where

import Types
import Plotting
import Interval
import Roots

--------------------------------------------------------------------------------
--IFS plotting.

toifs :: (Coefficient a) => IterCoeffs a -> Point -> IFS
toifs coeffs c = (toifs' coeffs c, [c])
    where toifs' coeffs' c' z = map (\cf -> cf*z*c' + 1) (map toComplex coeffs')
        --two normalisations, z -> (cf z c + 1) and z -> (zc + cf)

scaleFactors :: (Coefficient a) => Config a -> [Complex Double]
scaleFactors (Config ic (rx,_) d c eps _) = 
                filterClose (0.2) allscalings
                          -- ^^ random constant, tweaking necessary
    where allscalings = (map $ (negate . recip . (flip evaluate c)) . derivative . (map toComplex)) (canHaveRoots ic d cI)
          cI = c +! ((-eps):+(-eps),eps:+eps)

ifsCheatCounts :: (Coefficient a) => 
                  [Complex Double] -> [Polynomial a] -> Config a -> [Pixel]
ifsCheatCounts scales pols (Config _ res d c w _) = toCoords points res (0:+0) w
    where points' = map (\pol -> (evaluate pol c)) (map (map toComplex) pols)
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'
           
ifsIterates :: Iterations -> IFS -> [Complex Double]
ifsIterates 0 (fs,vals) = vals
ifsIterates n (fs,vals) = fs =<< (ifsIterates (n-1) (fs,vals))

ifsCounts :: (Coefficient a) => [Complex Double] -> IFS -> Config a -> [Pixel]
ifsCounts scales ifs (Config ic res d c w _) = toCoords points res (0:+0) w
    where points' = ifsIterates d ifs
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'
