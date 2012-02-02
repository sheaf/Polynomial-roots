module Roots where

import Overture
import Prelude ()
import Types
import Trees
import Data.Tree(flatten)
import Data.Maybe
import Interval
import Plotting
import Numeric.GSL.Polynomials(polySolve)
import Numeric.LinearAlgebra.LAPACK(eigOnlyR, eigOnlyC)
import Foreign.Storable(Storable)
import qualified Data.Packed.Matrix as M
import qualified Data.Packed.Vector as V

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
--Root finding.

--Method using GSL library. Currently not used.
findRoots' :: Polynomial Double -> [Root]
findRoots' p
    | length p' <= 1 = []
    | otherwise = polySolve p'
        where p' = reverse . dropWhile (==0) . reverse $ p

--Alternative using LAPACK.
findRoots :: Coefficient a => Polynomial a -> [Root]
findRoots p
    | length p' <= 1 = []
    | isNothing (toReal lead) = V.toList $ eigOnlyC (companion pc)
    | otherwise = V.toList $ eigOnlyR (companion pr)
        where p' = reverse . dropWhile (==0) . reverse $ p
              lead = fromJust $ last p'
              pr = map ((/(fromJust.toReal $ lead)).(fromJust.toReal)) p'
              pc = map ((/(toComplex lead)).toComplex) p'

--Gives the companion matrix of the given input polynomial, assumed monic.
companion :: (M.Element a, Storable a, Coefficient a) 
          => Polynomial a -> M.Matrix a
companion p = M.fromColumns vcols
    where n = (length p)-1
          p' = map negate $ take n p
          zs = replicate (n-1) 0 ++ [1]
          shift m l = take n . drop m $ fromJust $ cycle l
          cols = (map (\k -> shift k zs) $ reverse [0..(n-2)]) ++ [p']
          vcols = map V.fromList cols

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

getPolys :: (Coefficient a) => Config m a -> [Polynomial a]
getPolys (Config ic (rx, ry) d c w _) = canHaveRoots ic d cI
  where h = w * fromIntegral ry / fromIntegral rx
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

polyRoots :: (Coefficient a) => [Polynomial a] -> [RootPlot a]
polyRoots polys = (\p -> map (RootPlot p) . findRoots $ p) =<< polys

getRoots :: (Coefficient a) => Config m a -> [RootPlot a]
getRoots cfg = polyRoots $ getPolys cfg
