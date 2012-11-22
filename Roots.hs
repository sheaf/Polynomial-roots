{-# LANGUAGE TypeFamilies #-}

module Roots where

import Overture
import Prelude ()
import Data.Tree (flatten)
import Data.Maybe (fromJust)

--import Numeric.GSL.Polynomials(polySolve)
import Numeric.LinearAlgebra.LAPACK(eigOnlyR, eigOnlyC)

import Types
import Trees
import Interval
import Foreign.Storable(Storable)
import qualified Data.Packed.Matrix as M (Matrix, Element, fromColumns)
import qualified Data.Packed.Vector as V (fromList, toList)

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
{-
findRoots' :: Polynomial Double -> [Root]
findRoots' p
    | length p' <= 1 = []
    | otherwise = polySolve p'
        where p' = reverse . dropWhile (==0) . reverse $ p
-}

--Alternative using LAPACK.
findRoots :: Coefficient a => Polynomial a -> [Root]
findRoots p
    | length p' <= 1 = []
    | length p' == 2 = let [b,a] = p' in [- toComplex b / toComplex a]
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
    where n = length p - 1
          p' = map negate $ take n p
          cols = companion' n ++ [p']
          vcols = map V.fromList cols

--Companion matrix missing the last column
companion' :: Num a => Int -> [[a]]
companion' 1 = [[]]
companion' 2 = [[0,1]]
companion' n = nxt $ companion' (n-1)
    where nxt (l:ls) = (l ++ [0]) : map (0:) (l:ls)

--------------------------------------------------------------------------------
--Plotting sets of roots.

getPolys :: (Coefficient a) => Config c a -> [Polynomial a]
getPolys (Config ic (rx, ry) d c w _ _) = canHaveRoots ic d cI
  where h = w * fromIntegral ry / fromIntegral rx
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

polyRoots :: (Coefficient a) => [Polynomial a] -> [(Polynomial a, Root)]
polyRoots polys = (\p -> map (\r -> (p,r)) . findRoots $ p) =<< polys

getRoots :: (Coefficient a) => Config c a -> [(Polynomial a, Root)]
getRoots cfg = polyRoots $ getPolys cfg
