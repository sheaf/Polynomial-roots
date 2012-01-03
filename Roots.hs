module Roots where

import Types
import Interval
import Plotting
import Numeric.GSL.Polynomials(polySolve)

--------------------------------------------------------------------------------
--Finding polynomials with possible roots using tree pruning.
--At the moment just works for the iteration with coefficients in {1,-1},
--but this can easily be extended provided one has an appropriate bound.

bound :: (Coefficient a) => 
         IterCoeffs a -> Degree -> ComplexInterval -> RealInterval
bound coeffs d cI
    |1 `elemI` rI = (0,8) --lousy bound? works for other IterCoeffs??
    |(fst rI) > 1 = (0, maxcoeff * mini (1 /! rI))
    |otherwise = (0, maxcoeff * mini rI)
        where mini rI' = (snd rI')^(d+1) * snd (1 /! (1 -! rI'))
              rI = absI cI
              maxcoeff = maximum (map toAbs coeffs)

--Constructs next level in the tree of polynomials.
nextLevel :: (Coefficient a) => 
              IterCoeffs a -> CoefficientTree a -> CoefficientTree a
nextLevel _ Empty = Empty
nextLevel coeffs (Node c r []) = Node c r (map (\cf ->(Node cf False [])) coeffs)
nextLevel coeffs (Node c r t) = Node c r (map (nextLevel coeffs) t)

--Prunes the tree corresponding to whether values of the iterations of the
--given polynomial, prescribed by the tree, lie within the bound.
--Only checks this at leaves, where it also changes the CanHaveRoot values.
prune :: (Coefficient a) => 
          IterCoeffs a -> ComplexInterval -> Polynomial a 
           -> CoefficientTree a -> CoefficientTree a
prune _ _ _ Empty = Empty
prune coeffs cI p (Node c r [])
    | 1 `elemI` (absI cI) = Node c True []
    |(0 `elemI` values) = Node c True []
    |((absD $ values ) `intersects` (bound coeffs (length p) cI)) = Node c False []
        --note: length p is the degree of c:p
    |otherwise = Empty
        where dI = rectToDisk cI
              values = evaluateD (map toComplex $ p ++ [c]) dI
              --evaluateD prunes more efficiently than evaluateI
prune coeffs cI p (Node c r t)
    |(prunedT == []) = Empty
    |otherwise = Node c r prunedT
        where prunedT = filter (/= Empty) $ map (prune coeffs cI (p ++ [c])) t

--Constructs a pruned version of the tree of Littlewood polynomials.
constructTree :: (Coefficient a) => 
             IterCoeffs a -> Degree -> ComplexInterval -> CoefficientTree a
constructTree coeffs 0 _ = (Node 1 False [])
constructTree coeffs d cI = prune coeffs cI [] (nextLevel coeffs $ constructTree coeffs (d-1) cI)

--Returns the polynomials in the tree with CanHaveRoot = True.
getPolynomials :: (Coefficient a) => CoefficientTree a -> [Polynomial a]
getPolynomials Empty = []
getPolynomials (Node c False t) = map (c:) (concatMap getPolynomials t)
getPolynomials (Node c True t)= [[c]] ++ map (c:) (concatMap getPolynomials t)

getAllLeafPolynomials :: (Coefficient a) => CoefficientTree a -> [Polynomial a]
getAllLeafPolynomials Empty = []
getAllLeafPolynomials (Node c _ []) = [[c]]
getAllLeafPolynomials (Node c _ t)= map (c:) (concatMap getAllLeafPolynomials t)

--This gives the possible initial terms for polynomials that might have roots.
canYieldRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canYieldRoots coeffs d cI = getAllLeafPolynomials $ constructTree coeffs d cI

canHaveRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canHaveRoots coeffs d cI = getPolynomials $ constructTree coeffs d cI

--------------------------------------------------------------------------------
--Root finding: GSL library.

findRoots :: Polynomial Double -> [Root]
findRoots = polySolve

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

getPolys :: (Real a, Coefficient a) => Config c a -> [Polynomial a]
getPolys (Config ic (rx, ry) d c w _) = canHaveRoots ic d cI
  where h = (w * fromIntegral ry / fromIntegral rx) -- ::Double
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

polyRoots :: (Real a, Coefficient a) => [Polynomial a] -> [RootPlot a]
polyRoots polys = (\p -> map (RootPlot p) . findRoots $ map realToFrac p) =<< polys

getRoots :: (Real a, Coefficient a) => Config c a -> [RootPlot a]
getRoots cfg = polyRoots $ getPolys cfg
