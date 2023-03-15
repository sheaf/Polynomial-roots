module Polynomials where

-- polynomial-roots
import Types
  ( Polynomial, Coefficient(..) )

--------------------------------------------------------------------------------
--Some routines for polynomials.

trim :: Coefficient a => Polynomial a -> Polynomial a
trim = reverse . dropWhile (==0) . reverse

evaluate :: Coefficient a => Polynomial a -> a -> a
evaluate p z = foldr (\b w -> b + z * w) 0 p

derivative :: Coefficient a => Polynomial a -> Polynomial a
derivative = zipWith (*) (map fromIntegral [1 :: Int ..]) . drop 1

-- Gives the coefficients of the taylor expansion of p centered at c.
taylor :: (Coefficient a, Fractional a) =>
          Polynomial a -> a -> Polynomial a
taylor p c = zipWith (/) derivs facts
            where derivs = map (`evaluate` c) $ takeWhile (not.null) $ iterate derivative p
                  facts = map fromIntegral $ scanl (*) 1 [1 :: Int ..]

-- Finds the closest coefficient to a given one in a list.
closest :: Coefficient a => a -> [a] -> a
closest _  []     = error "finding closest element in an empty list..."
closest _  (c:[]) = c
closest cf (c:cs) = let c' = closest cf cs in
                        case toAbs (cf - c) < toAbs (cf - c') of
                              True -> c
                              _    -> c'
