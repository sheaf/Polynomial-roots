{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances,
             RankNTypes#-}
module Types where

import Overture
import Prelude ()

import Control.DeepSeq(NFData)

import Configuration hiding (center)
import Data.Complex
import Data.Ratio (Ratio, numerator, denominator)
import Rendering.Colour (AlphaColour)
import Util

--------------------------------------------------------------------------------
--Basic datatypes.

--Polynomials. Constant coefficient is the 0th term.
--Have to be able to coerce coefficients into the complex numbers,
--for polynomial evaluation.

-- |Class of allowable coefficients for polynomials.
class (Eq a, Num a, Show a, Read a, NFData a) => Coefficient a where
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
instance (Coefficient a, RealFloat a) => Coefficient (Complex a) where 
    toComplex (x :+ y) = toComplex x + (0 :+ 1) * toComplex y 
    toAbs     (x :+ y) = toAbs(x^2+y^2)
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
    (+)             = zipWithDefault (+) 0 0
    negate          = map negate
    (a:as) * (b:bs) = [a*b] + (0 : map (a*) bs) + (0: map (*b) as) + (0 : 0 : (as*bs))
    _      * _      = []
    fromInteger n   = [fromInteger n]
    abs             = map abs
    signum _        = error "No signum definition for Polynomial"

--Root finding types.
type Degree     = Int
type RealBound  = Degree -> Double
type Root       = Complex Double
    
--Plotting datatypes, and options.
type Resolution = (Int,Int)
type Center     = Complex Double
type Width      = Double
type Pixel      = (Int,Int)

--Scaling function
type Scaler a   = Complex Double -> Polynomial a -> Complex Double

--Gradients, backgrounds.
newtype Gradient m clr a = Grad { runGrad :: m -> clr a }
type BG = AlphaColour Double

data Config c a = Config { coefficients :: [a]
                         , resolution   :: Resolution
                         , degree       :: Degree
                         , center       :: Center
                         , width        :: Width
                         , scaling      :: Either Bool Double
                         , colouring    :: c
                         }

--Colouring schemes.
type SourceCol  a = (GradientSpec, BG, String, [a], Int) 
                 --gradient, background colour, method, coefficients, truncation
type SourceColB   = (GradientSpec, BG, String, Int) 
type DensityCol   = (GradientSpec, BG, Double) 
                 --gradient, background colour, density

addCfs :: [a] -> SourceColB -> SourceCol a
addCfs d (a,b,c,e) = (a,b,c,d,e)

-- |Colour schemes.
-- A colour scheme takes in input, turning it into elements of a monoid.
-- After using the monoid structure to combine input, it outputs a colour.
class (Monoid (ColourData c)) => ColourScheme c where
    type ColourData c :: *
    type InputData  c :: *
    -- |Turns input into values of a monoid.
    toData   :: c -> InputData  c -> ColourData c
    -- |Takes in a value of the monoid and returns a colour.
    toColour :: c -> ColourData c -> AlphaColour Double
    -- |Gives the corresponding coordinate for an input value.
    toCoord  :: c -> InputData  c -> Complex Double
    -- |Background colour; useful separately to toColour mempty.
    bg       :: c -> AlphaColour Double
