module Polynomials where

import Overture
import Prelude()

import Types (Polynomial, Coefficient)

--------------------------------------------------------------------------------
--Some routines for polynomials.

evaluate :: Coefficient a => Polynomial a -> a -> a
evaluate (a:as) z = a + z * evaluate as z
evaluate [] _ = 0

derivative :: Coefficient a => Polynomial a -> Polynomial a
derivative = zipWith (*) (map fromIntegral [1..]) . drop 1

--Gives the coefficients of the taylor expansion of p centered at c.
taylor :: (Coefficient a, Fractional a) => 
          Polynomial a -> a -> Polynomial a
taylor p c = zipWith (/) derivs facts 
            where derivs = map (`evaluate` c) $ takeWhile (not.null) $ iterate derivative p
                  facts = map fromIntegral $ scanl (*) 1 [1..]
