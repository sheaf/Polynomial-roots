{-# LANGUAGE ScopedTypeVariables #-}

module Trees where

-- containers
import Data.Tree
  ( Tree(Node), Forest )

-- polynomial-roots
import Interval
import Types

--------------------------------------------------------------------------------
-- Trees of coefficients as representing polynomials.

type CanHaveRoot = Bool

-- | A tree of values marked with an additional boolean.
-- Utility: flag which polynomials in a tree of polynomials can have a root,
-- in some specified region.
type BTree a = Tree (a, CanHaveRoot)

-- | Same as for trees, this marks values with an additional boolean.
type BForest a = Forest (a, CanHaveRoot)

-- | Returns the polynomials in the tree with CanHaveRoot = True.
getPolynomials :: BTree a -> [Polynomial a]
getPolynomials (Node (c,False) t) = map (c:) (concatMap getPolynomials t)
getPolynomials (Node (c,True) t) = [c] : map (c:) (concatMap getPolynomials t)

-- | Returns ALL polynomials in a tree corresponding to the leaves.
getAllLeafPolynomials :: BTree a -> [Polynomial a]
getAllLeafPolynomials (Node (c,_) []) = [[c]]
getAllLeafPolynomials (Node (c,_) t)= map (c:) (concatMap getAllLeafPolynomials t)

-- | Prunes away a forest to only leave the polynomials with CanHaveRoot = True.
toPolyForest :: Polynomial a -> BForest a -> Forest (Polynomial a)
toPolyForest _ [] = []
toPolyForest p (Node (c,b) f:ns) = let
    p' = p++[c]
    f' = toPolyForest p' f
    ns'= toPolyForest p ns in if b then Node p' f' : ns'
                                   else f' ++ ns'

-- | Returns the leaf forest of a given forest.
getLeafForest :: Forest a -> Forest a
getLeafForest [] = []
getLeafForest (Node c []:ns) = Node c [] : getLeafForest ns
getLeafForest (Node _ f:ns) = getLeafForest f ++ getLeafForest ns

-- | Constructs the next level in the tree of polynomials,
-- i.e. adds a child leaf to each leaf of the tree, for each coefficient.
nextLevel :: IterCoeffs a -> BTree a -> BTree a
nextLevel cfs (Node cb [])= Node cb (map (\cf -> Node (cf,True) []) cfs)
nextLevel cfs (Node cb t) = Node cb (map (nextLevel cfs) t)

-- | Same as for trees: constructs the next level in a forest of polynomials.
nextLevelF :: IterCoeffs a -> BForest a -> BForest a
nextLevelF cfs = map (nextLevel cfs)

-- | Prunes the forest corresponding to whether values of the iterations of the
-- given polynomial, prescribed by the forest, lie within the bound.
-- Only checks this at leaves, where it also changes the CanHaveRoot values.
pruneLeaves :: Coefficient a
            => RealBound -> ComplexInterval -> Polynomial a
            -> BForest a -> BForest a
pruneLeaves _ _ _ [] = []
pruneLeaves bound cI p (Node (c,_) []:ns)
    | 0 `elemI` vals
    = Node (c,True) []:ns'
    | absD vals `intersects` (0, bound $ length p)
    = Node (c,False) []:ns'
        --note: length p is the degree of c:p
    | otherwise
    = ns'
        where dI   = rectToDisk cI --disk arithmetic gives better results
              vals = evaluateD (map toComplex $ p ++ [c]) dI
              --(evaluateD prunes more efficiently than evaluateI)
              ns'  = pruneLeaves bound cI p ns
pruneLeaves bound cI p (Node (c,b) f:ns)
    --bypass, in case pruning can't work
    | 1 `elemI` absI cI
    = setCanHaveRoots cI p (Node (c,b) f) : ns'
    --end of bypass
    | otherwise
    = case f' of
        [] -> ns'
        _ -> Node (c,b) f' : ns'
    where ns' = pruneLeaves bound cI p ns
          f'  = pruneLeaves bound cI (p++[c]) f

-- | Changes the CanHaveRoots values at the leaves,
-- but doesn't bother to do any pruning.
setCanHaveRoots :: Coefficient a
                => ComplexInterval -> Polynomial a -> BTree a -> BTree a
setCanHaveRoots cI p (Node (c,_) [])
    | 0 `elemI` vals = Node (c,True ) []
    | otherwise      = Node (c,False) []
        where dI   = rectToDisk cI
              vals = evaluateD (map toComplex $ p ++ [c]) dI
setCanHaveRoots cI p (Node (c,b) f) = Node (c,b) f'
    where f' = map (setCanHaveRoots cI (p++[c])) f

-- | Constructs a pruned version of the tree of polynomials.
constructForest :: (Coefficient a) =>
                 Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                 -> BForest a
constructForest 0 _ _ _ = [Node (1,False) []]
constructForest d cfs bd cI = pruneLeaves bd cI [] . nextLevelF cfs
                            $ constructForest (d-1) cfs bd cI

-- | Given a forest of polynomials, this takes the leaves,
-- and then keeps growing from the leaves, pruning.
--
-- Returns a list of polynomials, with corresponding forests of coefficients.
continueForest :: forall a. (Coefficient a)
               => Degree -> IterCoeffs a -> RealBound -> ComplexInterval
               -> Forest (Polynomial a) -> [(Polynomial a, BForest a)]
continueForest 0 _ bd cI f =
  filter (not.null.snd) $ map sPrune $ getLeafForest f
  where
    sPrune :: Tree [a] -> ([a], BForest a)
    sPrune (Node p _) = (p', pruneLeaves bd cI p' [Node (c,True) []])
      where
        c
          | null p
          = error "empty polynomial in forest continuation"
          | otherwise
          = last p
        p' = (reverse . drop 1 . reverse) p

continueForest d cfs bd cI f
  = filter (not.null.snd)
  $ map sNextLevel
  $ continueForest (d-1) cfs bd cI f
    where
      sNextLevel (p,f2) = (p, pruneLeaves bd cI p f2')
        where f2' = nextLevelF cfs f2

continuePolyForest :: (Coefficient a)
                   => Degree -> IterCoeffs a -> RealBound -> ComplexInterval
                   -> Forest (Polynomial a) -> Forest (Polynomial a)
continuePolyForest d cfs bd cI f =
  concatMap (uncurry toPolyForest) $ continueForest d cfs bd cI f
