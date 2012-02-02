{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances #-}
module Types ( module Types
             , module Configuration
             , module Data.Complex
             ) where

import Overture
import Prelude ()
import Data.Complex
import Rendering.Colour
import Configuration hiding (center, Roots, IFS)

--------------------------------------------------------------------------------
--Basic datatypes.

--Polynomials. Constant coefficient is the 0th term.
--Have to be able to coerce coefficients into the complex numbers,
--for polynomial evaluation.
class (Num a, Read a) => Coefficient a where
    toComplex :: a -> Complex Double
    toAbs :: a -> Double
    toReal :: a -> Maybe Double

instance Coefficient Int where 
    toComplex = fromIntegral
    toAbs = abs . fromIntegral
    toReal = Just <$> fromIntegral
instance Coefficient Rational where
    toComplex = fromRational
    toAbs = abs . fromRational
    toReal = Just <$> fromRational
instance Coefficient Double where
    toComplex x = x :+ 0
    toAbs = abs
    toReal = Just <$> id
instance Coefficient (Complex Double) where 
    toComplex = id
    toAbs = magnitude
    toReal = const Nothing

--Polynomials as lists of coefficients.
type Polynomial a = [a]
type IterCoeffs a = [a]
type MyCoeff = Either Int (Complex Double)

instance (Coefficient a) => Num (Polynomial a) where
    a + b = zipWith (+) a b
    negate p = map negate p
    (a:as) * (b:bs) = [a*b] + (0 : map (a*) bs) + (0: map (*b) as) + (0 : 0 : (as*bs))
    _ * _ = []
    fromInteger n = [fromInteger n]
    abs p = map abs p
    signum _ = error "No signum definition for Polynomial"

--Root finding types.
type RealBound = Degree -> Double
type Point = Complex Double
type Guess = Complex Double
type Root = Complex Double
type Iterations = Int
type ErrorBound = Double

--Iterated function systems.
type IFS = (Point -> [Point], [Point]) --second coordinate is starting values
    
--Plotting datatypes, and options.
type Resolution = (Int,Int)
type Center = Complex Double
type Width = Double
type Pixel = (Int,Int)
--Scaling function
type Scaler a = Complex Double -> Polynomial a -> Complex Double
--Gradient as a monoid homomorphism into colour space.
newtype Gradient m clr a = Grad { runGrad :: m -> clr a }
data Mode = Roots | IFS | Both deriving (Eq, Ord, Read, Show)
data Config m a = Config { coefficients :: IterCoeffs a
                         , resolution   :: Resolution
                         , degree       :: Degree
                         , center       :: Center
                         , width        :: Width
                         , cgradient    :: Gradient m Colour Double
                         }

--------------------------------------------------------------------------------
--Basic functions.

evaluate :: (Coefficient a, b ~ a) => Polynomial a -> b -> b
evaluate (a:as) z = a + z * evaluate as z
evaluate [] _ = 0

derivative :: Coefficient a => Polynomial a -> Polynomial a
derivative = zipWith (*) (map fromIntegral [1..]) . drop 1

--A bit dodgy, but useful.
filterClose :: Coefficient a => ErrorBound -> [a] -> [a]
filterClose _ [] = []
filterClose eps (c:cs) = filterClose' eps cs [c]
    where filterClose' _ [] bs = bs
          filterClose' eps' (a:as) bs
              | good = filterClose' eps' as (bs++[a])
              | otherwise = filterClose' eps' as bs
                   where good = all (> eps') (map (\b -> toAbs (b-a)) bs)
