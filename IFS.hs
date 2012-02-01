module IFS where

import Overture
import Prelude ()
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


--Two somewhat outdated routines, computing global scale factors...
scaleFactors :: (Coefficient a) => Config m a -> [Complex Double]
scaleFactors (Config ic _ d c eps _) = 
                filterClose 0.1 allscalings
                         -- ^^ random constant, tweaking necessary
    where allscalings = (map $ (negate . recip . (`evaluate` c)) . derivative . map toComplex) (canHaveRoots ic d cI)
          cI = c +! ((-eps):+(-eps),eps:+eps)

getScales :: (Coefficient a) => Config m a -> [Complex Double]
getScales (Config ic (rx,ry) d c w g) = scales
  where scales = scaleFactors (Config ic (rx,ry) (d+8) c (w/ fromIntegral rx) g)  
            -- bear in mind scaleFactors uses w as an error bound...


          
scalePoint :: (Coefficient a) => Complex Double -> Scaler a -- Scaler a ~ (Complex Double -> Polynomial a -> Complex Double)
scalePoint c z p = z * scale
    where scale = (negate . recip . (`evaluate` c)) . derivative . map toComplex $ p

ifsCheatCounts :: (Coefficient a) => Config m a -> Scaler a -> [Polynomial a] -> [IFSPlot a]
ifsCheatCounts (Config _ _ _ c _ _ ) f ps = map IFSPlot points
    where points = map (\p -> f (flip evaluate c . map toComplex $ p) p) ps

ifsIterates :: Iterations -> IFS -> [Complex Double]
ifsIterates 0 (_,vals) = vals
ifsIterates n (fs,vals) = fs =<< ifsIterates (n-1) (fs,vals)

ifsCounts :: (Coefficient a) => [Complex Double] -> IFS 
          -> Config m a -> [Complex Double]
ifsCounts scales ifs (Config _ res d _ _ _) = points
    where points' = ifsIterates d ifs
          points = case scales of
                        [] -> points'
                        _ -> (\x -> map (x*) scales) =<< points'

ifsPoints :: (Coefficient a) => Config m a -> [IFSPlot a]
ifsPoints cfg@(Config ic (rx,ry) d c w g) = ifspoints
  where --scales = getScales cfg
        --ifs = toifs ic c
        --ifspoints = ifsCounts scales ifs (Config ic (rx,ry) (d+1) c w g)
        h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        --cI = ((-8):+(-8),8:+8) --no cheating, don't do any pruning!
        pols = canHaveRoots ic d cI
        ifspoints = ifsCheatCounts (Config ic (rx,ry) d c w g) (scalePoint c) pols 
        --ifspoints = ifsCheatCounts (Config ic (rx,ry) d c w g) (\z p -> z) pols
