module Polynomials where

import Overture
import Prelude()

import Types (Polynomial, Coefficient)

import Foreign.Storable(Storable)
import qualified Data.Packed.Matrix as M (Matrix, Element, fromColumns)
import qualified Data.Packed.Vector as V (Vector, fromList)

--------------------------------------------------------------------------------
--Some routines for polynomials.

evaluate :: Coefficient a => Polynomial a -> a -> a
evaluate p z = foldr' (\b w -> b + z * w) 0 p

derivative :: Coefficient a => Polynomial a -> Polynomial a
derivative = zipWith (*) (map fromIntegral [1..]) . drop 1

--Gives the coefficients of the taylor expansion of p centered at c.
taylor :: (Coefficient a, Fractional a) => 
          Polynomial a -> a -> Polynomial a
taylor p c = zipWith (/) derivs facts 
            where derivs = map (`evaluate` c) $ takeWhile (not.null) $ iterate derivative p
                  facts = map fromIntegral $ scanl (*) 1 [1..]

--------------------------------------------------------------------------------
--Routines to compute companion matrices.

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
