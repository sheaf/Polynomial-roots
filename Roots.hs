module Roots where

import Overture
import Prelude ()
import Types
import Trees
import Data.Tree(flatten)
import Interval
import Plotting
import Numeric.GSL.Polynomials(polySolve)

--------------------------------------------------------------------------------
--Bound used for pruning trees of polynomials.

bound :: (Coefficient a) => 
         IterCoeffs a -> ComplexInterval -> RealBound
bound cfs cI d
    |1 `elemI` rI = 8 --lousy bound? works for other IterCoeffs?
    |fst rI > 1 = maxcoeff * mini (1 /! rI)
    |otherwise = maxcoeff * mini rI
        where mini rI' = (snd rI')^(d+1) * snd (1 /! (1 -! rI'))
              rI = absI cI
              maxcoeff = maximum (map toAbs cfs)

--This gives the possible initial terms for polynomials that might have roots.
canYieldRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canYieldRoots cfs d cI = concatMap getAllLeafPolynomials $ constructForest d cfs (bound cfs cI) cI

canHaveRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
--canHaveRoots cfs d cI = concatMap getPolynomials $ constructForest d cfs (bound cfs cI) cI
canHaveRoots cfs d cI = concatMap flatten $ constructPolyForest d cfs (bound cfs cI) cI

--------------------------------------------------------------------------------
--Root finding: GSL library.
findRoots :: Polynomial Double -> [Root]
findRoots p
    | length p' <= 1 = []
    | otherwise = polySolve p'
        where p' = reverse . dropWhile (==0) . reverse $ p

--------------------------------------------------------------------------------
--Plotting sets of roots.

{- Useless, and also outdated code.
   Evaluates the polynomials at pixel-wide complex intervals,
   and returns how many of the resulting complex intervals contain 0.
   Produces blocky "spread out" versions of the real images.

colourFunction' :: (Coefficient a) => 
                   [Polynomial a] -> Config a ->  Pixel -> Colour
colourFunction' polys (Config _ (rx,ry) w c (grad,_)) (px,py) = col
    where cI = (((realPart c -w/2):+ (imagPart c -h/2)),
                ((realPart c +w/2):+ (imagPart c +h/2)))
          h = w * ry'/rx'
          roots = filter (\pol -> 0 `elemI` (evaluateI pol pI)) polys
          pI = (((px'-0.5)*w/rx':+(py'-0.5)*h/ry'),
                ((px'+0.5)*w/rx':+(py'+0.5)*h/ry')) !+ (fst cI)
          [px',py',rx',ry'] = map (fromIntegral) [px,py,rx,ry]
          col = grad (length roots)
-}

getPolys :: (Real a, Coefficient a) => Config m a -> [Polynomial a]
getPolys (Config ic (rx, ry) d c w _) = canHaveRoots ic d cI
  where h = w * fromIntegral ry / fromIntegral rx
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

polyRoots :: (Real a, Coefficient a) => [Polynomial a] -> [RootPlot a]
polyRoots polys = (\p -> map (RootPlot p) . findRoots $ map realToFrac p) =<< polys

getRoots :: (Real a, Coefficient a) => Config m a -> [RootPlot a]
getRoots cfg = polyRoots $ getPolys cfg
