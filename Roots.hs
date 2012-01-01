module Roots where

import Types
import Interval
import Plotting

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
    |(0 `elemI` values) = Node c True []
    |((absI $ values ) `intersects` (bound coeffs (length p) cI)) = Node c False []
        --note: length p is the degree of c:p
    |otherwise = Empty
        where values = evaluateI (map toComplex $ p ++ [c]) cI
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
--Root finding algorithm using Laguerre's method.

--One iteration of Laguerre's method.
laguerre' :: (Coefficient a) => Polynomial a -> Guess -> Guess
laguerre' q x = x - a
    where p = map toComplex q
          g = (evaluate p' x) / (evaluate p x)
          h = g^2 - (evaluate p'' x) / (evaluate p' x)
          k = sqrt((n-1)*(n*h - g^2))
          s
            | magnitude (g+k) >= magnitude (g-k) = g+k
            | otherwise = g-k
          a = n / s
          n = fromIntegral $ length p -1
          p' = derivative p
          p''= derivative p'

laguerre :: (Coefficient a) => 
        ErrorBound -> Iterations -> Polynomial a -> Guess -> Root
laguerre eps k p g
    | k <= 0 = g
    | magnitude (g' - g) < eps = g' --not a very robust check!
    | otherwise = laguerre eps (k-1) p g'
        where g' = laguerre' p g

--Deflates a polynomial, i.e. divides it by a monic factor.
deflate :: (Coefficient a) => Guess -> Polynomial a -> Polynomial (Complex Double)
deflate g p = tail $ deflate' g as bs
    where [as,bs] = map (map toComplex) [take 1 $ reverse p, drop 1 $ reverse p]
          deflate' z as' bs'
            | bs' == [] = as'
            | otherwise = deflate' z (((head bs')+z*(head as')):as') (tail bs')

findRoots :: (Coefficient a) => 
             ErrorBound -> Iterations -> Polynomial a -> [Root]
findRoots _   _ []     = error "polynomial with no coefficients"
findRoots _   _ [0]    = [0]
findRoots _   _ [_]    = []
findRoots eps k [b, 0] = findRoots eps k [b]
findRoots eps k [b, a] = [ toComplex (-b) / toComplex a]
findRoots eps k p      = z : findRoots eps k (deflate z p2)
    where z = laguerre eps k p2 0
          p2 = reverse $ (dropWhile (==0)) $ reverse p


--------------------------------------------------------------------------------
--Plotting sets of roots.

{- Useless, and also outdated code.
   Evaluates the polynomials at pixel-wide complex intervals,
   and returns how many of the resulting complex intervals contain 0.
   Produces blocky "spread out" versions of the real images.

colourFunction' :: (Coefficient a) => 
                   [Polynomial a] -> Resolution -> Width -> Center
                   -> Gradient -> Pixel -> Colour
colourFunction' polys (rx,ry) w c (grad,_) (px,py) = col
    where cI = (((realPart c -w/2):+ (imagPart c -h/2)),
                ((realPart c +w/2):+ (imagPart c +h/2)))
          h = w * ry'/rx'
          roots = filter (\pol -> 0 `elemI` (evaluateI pol pI)) polys
          pI = (((px'-0.5)*w/rx':+(py'-0.5)*h/ry'),
                ((px'+0.5)*w/rx':+(py'+0.5)*h/ry')) !+ (fst cI)
          [px',py',rx',ry'] = map (fromIntegral) [px,py,rx,ry]
          col = grad (length roots)
-}

rootList :: (Coefficient a) => 
            [Polynomial a] -> Resolution -> Center -> Width -> [Pixel]
rootList polys (rx,ry) c w  = coordlist
    where rx' = fromIntegral rx
          rootlist= concat $ map (findRoots (0.5*w/rx') 300)
                           $ map (map toComplex) polys
          coordlist = toCoords rootlist (rx,ry) c w
