{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Roots where

import Overture
import Prelude ()

import Data.Functor.Compose
import Data.Tree (Forest)
import Data.Maybe (fromJust)
import Polynomials(companion, trim)

--import Numeric.GSL.Polynomials(polySolve)
import Numeric.LinearAlgebra.LAPACK(eigOnlyR, eigOnlyC)

import Types
import Trees
import Interval
import qualified Data.Packed.Vector as V (toList)

--------------------------------------------------------------------------------
--Bound used for pruning trees of polynomials.

-- |Bounds the contribution of the remaining terms past an initial segment.
-- This is used to prune trees of polynomials.
bound :: (Coefficient a) => IterCoeffs a -> ComplexInterval -> RealBound
bound cfs cI d
    |fst rI > 1 = maxcoeff * mini (1 /! rI)
    |otherwise = maxcoeff * mini rI
        where mini rI' = (snd rI')^(d+1) * snd (1 /! (1 -! rI'))
              rI = absI cI
              maxcoeff = maximum (map toAbs cfs)

-- |Gives the possible initial segments of polynomials that can have roots.
canYieldRoots :: (Coefficient a) =>
              IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canYieldRoots cfs d cI = concatMap getAllLeafPolynomials 
                       $ constructForest d cfs (bound cfs cI) cI

-- |Returns all polynomials up to the given degree which can have roots
-- in the specified region.
canHaveRoots :: (Coefficient a) =>
             IterCoeffs a -> Degree -> ComplexInterval -> Forest (Polynomial a)
canHaveRoots cfs d cI = toPolyForest [] $constructForest d cfs (bound cfs cI) cI

--------------------------------------------------------------------------------
--Root finding.

{---Method using GSL library. Currently not used.
findRoots, findRoots' :: Polynomial Double -> [Root]
findRoots = findRoots' . trim
findRoots' p
    | length p <= 1 = []
    | otherwise     = polySolve p
-}

--| Root finding methods, using LAPACK.
findRoots, findRoots' :: Coefficient a => Polynomial a -> [Root]
findRoots = findRoots' . trim
findRoots' p
    | length p <= 1           = []
    | length p == 2           = let [b,a] = p in [- toComplex b / toComplex a]
    | isNothing (toReal lead) = V.toList $ eigOnlyC (companion pc)
    | otherwise               = V.toList $ eigOnlyR (companion pr)
        where lead = fromJust $ last p
              pr   = map ((/(fromJust.toReal $ lead)).(fromJust.toReal)) p
              pc   = map ((/(toComplex lead)).toComplex) p

--------------------------------------------------------------------------------
--Plotting sets of roots.

-- |Returns the forest of polynomials that can have roots in a region.
getPolys :: (Coefficient a) => Config c a -> Forest (Polynomial a)
getPolys (Config ic (rx, ry) d c w _ _) = canHaveRoots ic d cI
  where h  = w * fromIntegral ry / fromIntegral rx
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

-- |Returns all possible roots that can occur in a region, up to a given degree.
getRoots :: (Coefficient a) => ((Polynomial a -> [Root]) -> (Polynomial a -> b))
         -> Config c a -> Forest b
getRoots q cfg = getCompose . fmap (q findRoots) . Compose $ getPolys cfg
