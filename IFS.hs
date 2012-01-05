module IFS where

import Types
import Plotting
import Interval
import Roots

--------------------------------------------------------------------------------
--IFS plotting.

toifs :: (Coefficient a) => IterCoeffs a -> Point -> IFS
toifs coeffs c = (toifs' coeffs c, [c])
    where toifs' coeffs' c' z = map ((\cf -> cf*z*c' + 1) . toComplex) coeffs'
        --two normalisations, z -> (cf z c + 1) and z -> (zc + cf)

scaleFactors :: (Coefficient a) => Config c a -> [Complex Double]
scaleFactors (Config ic (rx,_) d c eps _) = 
                filterClose 0.1 allscalings
                         -- ^^ random constant, tweaking necessary
    where allscalings = (map $ (negate . recip . (`evaluate` c)) . derivative . map toComplex) (canHaveRoots ic d cI)
          cI = c +! ((-eps):+(-eps),eps:+eps)

ifsCheatCounts :: (Coefficient a) => 
                  [Complex Double] -> [Polynomial a] -> Config c a -> [Complex Double]
ifsCheatCounts scales pols (Config _ res d c w _) = points
    where points' = map ((`evaluate` c) . map toComplex) pols
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'

ifsIterates :: Iterations -> IFS -> [Complex Double]
ifsIterates 0 (fs,vals) = vals
ifsIterates n (fs,vals) = fs =<< ifsIterates (n-1) (fs,vals)

ifsCounts :: (Coefficient a) => [Complex Double] -> IFS -> Config c a -> [Complex Double]
ifsCounts scales ifs (Config ic res d c w _) = points
    where points' = ifsIterates d ifs
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'

getScales :: (Coefficient a) => Config c a -> [Complex Double]
getScales (Config ic (rx,ry) d c w g) = scales
  where scales = scaleFactors (Config ic (rx,ry) (d+8) c (w/ fromIntegral rx) g)  
            -- bear in mind scaleFactors uses w as an error bound...

ifsPoints :: (Coefficient a) => Config c a -> [IFSPlot a]
ifsPoints cfg@(Config ic (rx,ry) d c w g) = map IFSPlot ifspoints
  where scales = getScales cfg
        ifs = toifs ic c
        ifspoints = ifsCounts scales ifs (Config ic (rx,ry) (d+1) c w g)
        --h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        --cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        --pols = canHaveRoots ic d cI
        --ifspoints = ifsCheatCounts scales pols (Config ic (rx,ry) d c w g)

