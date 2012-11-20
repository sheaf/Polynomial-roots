{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances,
             RankNTypes#-}
module Types ( module Types
             , module Configuration
             , module Data.Complex
             ) where

import Overture
import Prelude ()

import Configuration hiding (center)
import Data.Complex
import Data.Monoid
import Data.Ratio
import Rendering.Colour

--------------------------------------------------------------------------------
--Basic datatypes.

--Polynomials. Constant coefficient is the 0th term.
--Have to be able to coerce coefficients into the complex numbers,
--for polynomial evaluation.

class (Eq a, Num a, Show a, Read a) => Coefficient a where
    toComplex :: a -> Complex Double
    toAbs     :: a -> Double
    toReal    :: a -> Maybe Double

instance Coefficient Int where 
    toComplex = fromIntegral
    toAbs     = abs . fromIntegral
    toReal    = Just <$> fromIntegral
instance Coefficient Integer where
    toComplex = fromIntegral
    toAbs     = abs . fromIntegral
    toReal    = Just <$> fromIntegral
instance Coefficient Double where
    toComplex x = x :+ 0
    toAbs       = abs
    toReal      = Just <$> id
instance Coefficient (Complex Double) where 
    toComplex = id
    toAbs     = magnitude
    toReal    = const Nothing
instance (Coefficient a, Integral a) => Coefficient (Ratio a) where
    toComplex q = toComplex a / toComplex b
        where (a,b) = (numerator q, denominator q)
    toAbs     q = toAbs a / toAbs b
        where (a,b) = (numerator q, denominator q)
    toReal    q = (/) <$> toReal a <*> toReal b
        where (a,b) = (numerator q, denominator q)

--Polynomials as lists of coefficients.
type Polynomial a = [a]
type IterCoeffs a = [a]

instance (Coefficient a) => Num (Polynomial a) where
    a + b           = zipWith (+) a b
    negate p        = map negate p
    (a:as) * (b:bs) = [a*b] + (0 : map (a*) bs) + (0: map (*b) as) + (0 : 0 : (as*bs))
    _ * _           = []
    fromInteger n   = [fromInteger n]
    abs p           = map abs p
    signum _        = error "No signum definition for Polynomial"

--Root finding types.
type Degree     = Int
type RealBound  = Degree -> Double
type Point      = Complex Double
type Guess      = Complex Double
type Root       = Complex Double
type Iterations = Int
type ErrorBound = Double
type StartVals  = [Point]

--Iterated function systems.
type IFS = (Point -> [Point], StartVals)
    
--Plotting datatypes, and options.
type Resolution = (Int,Int)
type Center     = Complex Double
type Width      = Double
type Pixel      = (Int,Int)
--Scaling function
type Scaler a   = Complex Double -> Polynomial a -> Complex Double
--Gradient as a monoid homomorphism into colour space.
newtype Gradient m clr a = Grad { runGrad :: m -> clr a }

data Config c a = Config { coefficients :: [a]
                         , resolution   :: Resolution
                         , degree       :: Degree
                         , center       :: Center
                         , width        :: Width
                         , colouring    :: c
                         }

--Colouring schemes.
type SourceCol  a = (String, [a], Double) --method, coefficients, opacity
type DensityCol   = (GradientSpec, Double) --gradient, density

class (Monoid (ColourData c)) => ColourScheme c where
    type ColourData c :: *
    type InputData  c :: *
    toColour :: c -> (ColourData c) -> AlphaColour Double
    toData   :: c -> InputData c -> ColourData c
    toCoord  :: c -> InputData c -> Complex Double
