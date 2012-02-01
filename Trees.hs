module Trees where

import Overture
import Prelude ()
import Types
import Interval
import Data.Tree
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
--Trees of coefficients as representing polynomials.

type CanHaveRoot = Bool
type BTree a = Tree (a, CanHaveRoot)
type BForest a = Forest (a, CanHaveRoot)

--Returns the polynomials in the tree with CanHaveRoot = True.
getPolynomials :: BTree a -> [Polynomial a]
getPolynomials (Node (c,False) t) = map (c:) (concatMap getPolynomials t)
getPolynomials (Node (c,True) t)= [c] : map (c:) (concatMap getPolynomials t)

getAllLeafPolynomials :: BTree a -> [Polynomial a]
getAllLeafPolynomials (Node (c,_) []) = [[c]]
getAllLeafPolynomials (Node (c,_) t)= map (c:) (concatMap getAllLeafPolynomials t)

--Returns a forest of polynomials with CanHaveRoot = True
toPolyForest :: Polynomial a -> BForest a -> Forest (Polynomial a)
toPolyForest _ [] = []
toPolyForest p (Node (c,b) f:ns) = let 
    p' = p++[c]
    f' = toPolyForest p' f
    ns'= toPolyForest p ns in if b then Node p' f' : ns'
                                   else f' ++ ns'

getLeafForest :: Forest a -> Forest a
getLeafForest [] = []
getLeafForest (Node c []:ns) = Node c [] : getLeafForest ns 
getLeafForest (Node _ f:ns) = getLeafForest f ++ getLeafForest ns

--Constructs next level in the tree of polynomials.
nextLevel :: IterCoeffs a -> BTree a -> BTree a
nextLevel cfs (Node cb [])= Node cb (map (\cf -> Node (cf,True) []) cfs)
nextLevel cfs (Node cb t) = Node cb (map (nextLevel cfs) t)

nextLevelF :: IterCoeffs a -> BForest a -> BForest a
nextLevelF cfs = map (nextLevel cfs)

--Prunes the forest corresponding to whether values of the iterations of the
--given polynomial, prescribed by the forest, lie within the bound.
--Only checks this at leaves, where it also changes the CanHaveRoot values.
pruneLeaves :: (Coefficient a) => 
          RealBound -> ComplexInterval -> Polynomial a 
          -> BForest a -> BForest a
pruneLeaves _ _ _ [] = []
pruneLeaves bound cI p (Node (c,_) []:ns)
    | 0 `elemI` values = Node (c,True) []:ns'
    | absD values `intersects` (0, bound $ length p) = Node (c,False) []:ns'
        --note: length p is the degree of c:p
    | otherwise = ns'
        where dI = rectToDisk cI --disk arithmetic gives better results
              values = evaluateD (map toComplex $ p ++ [c]) dI
              --(evaluateD prunes more efficiently than evaluateI)
              ns' = pruneLeaves bound cI p ns
pruneLeaves bound cI p (Node (c,b) f:ns)
    --bypass, in case pruning can't work
    | 1 `elemI` absI cI = Node (c,True) f : ns'
    --end of bypass
    | otherwise = case f' of
                       [] -> ns'
                       _ -> Node (c,b) f' : ns'
        where ns' = pruneLeaves bound cI p ns
              f' = pruneLeaves bound cI (p++[c]) f

--Constructs a pruned version of the tree of polynomials.
constructForest :: (Coefficient a) => 
                 Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                 -> BForest a
constructForest 0 _ _ _ = [Node (1,False) []]
constructForest d cfs bd cI = pruneLeaves bd cI [] . nextLevelF cfs $ constructForest (d-1) cfs bd cI

constructPolyForest :: (Coefficient a) =>
                 Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                 -> Forest (Polynomial a)
constructPolyForest d cfs bd cI = toPolyForest [] $ constructForest d cfs bd cI

--Given a forest of polynomials, this takes the leaves,
--and then keeps growing from the leaves, pruning.
--Returns a list of polynomials, with corresponding forests of coefficients.
continueForest :: (Coefficient a) =>
                  Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                  -> Forest (Polynomial a) -> [(Polynomial a, BForest a)]
continueForest 0 _ bd cI f = filter (not.null.snd) $ map sPrune $ getLeafForest f
                                 where sPrune (Node p _) = (p', pruneLeaves bd cI p' [Node (c,True) []]) 
                                        where c = case last p of
                                                       Nothing -> error "empty polynomial in forest continuation"
                                                       Just lp -> lp
                                              p'= (reverse . drop 1 . reverse) p
continueForest d cfs bd cI f = filter (not.null.snd) $
                               map sNextLevel $
                               continueForest (d-1) cfs bd cI f 
                                 where sNextLevel (p,f2) = (p, pruneLeaves bd cI p f2')
                                        where f2' = nextLevelF cfs f2

continuePolyForest :: (Coefficient a) =>
                      Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                      -> Forest (Polynomial a) -> Forest (Polynomial a)
continuePolyForest d cfs bd cI f = concatMap (uncurry toPolyForest) $ continueForest d cfs bd cI f
