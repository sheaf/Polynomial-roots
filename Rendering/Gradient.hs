{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Rendering.Gradient where

import Overture
import Prelude ()

import Data.Colour.Names
import Data.List ( lookup )
import Data.Maybe ( fromJust )
import qualified Data.Map as Map (lookup)
import Data.Monoid

import Polynomials
import Rendering.Colour
import qualified Rendering.Colour.Names as Names (gradientDict, gradientNames)
import Types

apGrad :: m -> Gradient m f a -> f a
apGrad = flip runGrad

instance (AffineSpace f) => AffineSpace (Gradient m f) where
    affineCombo xs z = Grad $ \n -> calcGrad n
      where calcGrad n = affineCombo (second (apGrad n) <$> xs) (apGrad n z)

instance (ColourOps f) => ColourOps (Gradient m f) where
    over c (Grad f) = Grad $ \n -> over c (f n)
    darken s (Grad f) = Grad $ \n -> darken s (f n)

instance (Monoid (f a)) => Monoid (Gradient m f a) where
    mempty = constant unit
    mappend (Grad g1) (Grad g2) = Grad $ \n -> mappend (g1 n) (g2 n)

constant :: f a -> Gradient m f a
constant = Grad . const

linear :: (Monoid (f a), Fractional a, Ord a, AffineSpace f) => a -> a -> f a -> f a -> Gradient a f a
linear a1 a2 c1 c2 = Grad (\n -> case (n<a1', n>a2') of
                                      (True,_) -> mempty
                                      (_,True) -> mempty
                                      (False,False) -> case (a2' - a1') of
                                                            0 -> c1
                                                            d -> blend ((n-a1')/d) c2 c1
                          )                                   -- invert arguments of blend!!
    where (a1',a2') = (min a1 a2, max a1 a2)

onInput :: (m -> n) -> Gradient n f a -> Gradient m f a
onInput f g = Grad { runGrad =  runGrad g . f }

onOutput :: (f a -> g a) -> Gradient m f a -> Gradient m g a
onOutput f = Grad . (f .) . runGrad

reverseGrad, invert, square, squareRoot :: Gradient Double f a -> Gradient Double f a
reverseGrad = onInput (\x -> 1-x)
invert = onInput recip
square = onInput (^ 2)
squareRoot = onInput sqrt

adjacent :: Ord m => m -> Gradient m f a -> Gradient m f a -> Gradient m f a
adjacent x g1 g2 = Grad (\n -> runGrad (if n < x then g1 else g2) n)

--Takes a list of colours and control points, giving the corresponding gradient.
collate :: (Monoid (f a), Ord a, Fractional a, AffineSpace f) => [(f a, a)] -> Gradient a f a
collate cvs1 = Grad $ blender cvs'
    where cvs2 = sortBy (comparing snd) cvs1
          cvs3 = filter (\(_,b) -> (b >= 0 && b <= 1)) cvs2
          cvs' = case (head cvs3, last cvs3) of
                      (Just (_,0),Just (_,1)) -> cvs3
                      (Just (_,0),Just (b,_)) -> cvs3 ++ [(b,1)]
                      (Just (a,_),Just (_,1)) -> [(a,0)] ++ cvs3
                      (Just (a,_),Just (b,_)) -> [(a,0)] ++ cvs3 ++ [(b,1)]
                      _ -> error "could not collate, too few control points..."
          blender cvs n = blend n' c2 c1 -- arguments inverted again!!
              where n2 = min 1 . max 0 $ n
                    (c1,a1) = fromJust $ head $ dropWhile (\(a,b) -> b > n2) (reverse cvs)
                    (c2,a2) = fromJust $ head $ dropWhile (\(a,b) -> b < n2) cvs 
                    n' = if a1==a2 then n2 else (n2-a1)/(a2-a1)

withOpacityG :: Num a => a -> Gradient a Colour a -> Gradient a AlphaColour a
withOpacityG o = onOutput (`withOpacity` o)

collateWithOp o = withOpacityG o . collate

opacify :: (Num a) => Colour a -> Gradient m AlphaColour a -> Gradient m Colour a
opacify bg = onOutput (`over` bg)

fadeIn c = linear (opaque c) transparent
fadeOut c = linear transparent (opaque c)

monochrome = withOpacity
monochrome' o c = Grad { runGrad = runGrad (monochrome' o c)}
hsvGrad' o d = flip withOpacity o $ hsv d 1 1
hsvGrad o = Grad { runGrad = hsvGrad' o }

gradientByName' :: String -> (Double -> Gradient Double AlphaColour Double)
gradientByName' "hsv" = hsvGrad
gradientByName' s = case Map.lookup s Names.gradientDict of
                         Just g -> flip collateWithOp g
                         _      -> error $ "unrecognised gradient name: " ++ s

gradientNames = "hsv" : Names.gradientNames

gradientByName (s,d) = case d of
                            Just d' -> Just $ gradientByName' s d'
                            Nothing -> Just $ gradientByName' s 1

gradientFromSpec def bg gSpec = opacify bg $ fromExpr gSpec ?? def

fromExpr (NamedGradient g) = gradientByName g
fromExpr (Split gns) = splitGrads gns
fromExpr (Combine f xs) = combineGrads (getBlendFunc f) =<< mapM fromExpr xs
fromExpr (Collate cols) = Just $ collate cols

combineGrads _ [] = Nothing
combineGrads f [x] = Just x
combineGrads f (x:xs) = Just $ foldl f x xs

splitGrads [] = Just $ constant mempty
splitGrads ((g1, n1):gns) = adjacent n1 <$> g1' <*> splitGrads gns
  where g1' = fromExpr g1

getBlendFunc Blend = blend 0.5
getBlendFunc Overlay = (++)

newtype WeightedAlphaColour a = W (AlphaColour a, Int)
fromWeighted :: (Eq a, Num a, Fractional a) 
             => WeightedAlphaColour a -> AlphaColour a
fromWeighted (W(c,w)) = go c w
    where go c 0 = mempty
          go c 1 = c
          go c n = case alphaChannel c of
                        0 -> mempty
                        a -> dissolve ((1 - (1-a)^n)/a) c

instance Fractional a => Monoid (WeightedAlphaColour a) where
    mempty = W (transparent, 0)
    (W(c1,w1)) `mappend` (W(c2, w2)) = W (blend (v1 / (v1+v2)) c1 c2, w1+w2)
        where [v1,v2] = map fromIntegral [w1,w2]

--Colour scheme definitions.
instance (Coefficient a) => ColourScheme (SourceCol a) where
    type ColourData (SourceCol a) = WeightedAlphaColour Double
    type InputData  (SourceCol a)= (Polynomial a, Complex Double)
    toColour _              = fromWeighted
    toData   (s,_,"1",l,t)  = \(p,r) -> W (source1 g l (drop t p) r, 1)
        where g = fromJust $ fromExpr s
    toData   (s,_,"2",l,t)  = \(p,r) -> W (source2 g l (drop t p) r, 1)
        where g = fromJust $ fromExpr s
    toData   _              = error "wrong method for source colouring"
    toCoord  _ (_,z)        = z
    bg (_,col,_,_,_)        = col

instance ColourScheme DensityCol where
    type ColourData DensityCol = (Sum Double)
    type InputData  DensityCol = (Complex Double)
    toColour (spec,_,_) = runGrad (fromJust $ fromExpr spec) . getSum
    toData   (_   ,_,d) = density d
    toCoord  _ z        = z
    bg       (_,col,_)  = col

--Density colouring.
density :: Double -> r -> Sum Double
density d _ = Sum d

--Colouring by source polynomial, "base n" and "scale factor" methods.
source1, source2 :: (Coefficient a) => Gradient Double AlphaColour Double
                 -> IterCoeffs a -> Polynomial a 
                 -> Complex Double -> AlphaColour Double
source1 g cfs p _ = runGrad g (toGValue1 cfs p  )
source2 g cfs p r = runGrad g (toGValue2 cfs p r)

--------------------------------------------------------------------------------
--Converting polynomials to values to be able to apply gradients.

--Converts a polynomials with coefficients in [c1,c2,...] to a polynomial
--with coefficients in [d1,d2,...].
--Somewhat dodgy, as it depends on exact matching of coefficients...
convertCoeffs :: (Coefficient a, Coefficient b) => [a] -> [b] -> Polynomial a -> Polynomial b 
convertCoeffs _ _ [] = []
convertCoeffs cfs dfs (c:cs) = case lookup c cfs' of
                                    Nothing  -> error "couldn't look up coeff; possible rounding error"
                                    Just cf' -> cf' : convertCoeffs cfs dfs cs
    where cfs' = zipWith (\x y -> (y,x)) dfs cfs

--Note: colouring depends on the order of the coefficients.
--This is a "base d" expansion.
toGValue1 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Double
toGValue1 cfs p = z * evaluate p' z
    where d  = fromIntegral (length cfs) -1
          p' = convertCoeffs cfs [0..d] p
          z  = 1 / fromIntegral (length cfs)

--This one should be used with gradients such that g(0)=g(1).
--This uses the scale factors.
toGValue2 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Center -> Double
toGValue2 cfs p c = 0.5 + 1/(2*pi) * phase scale
    where scale = (negate . recip . (`evaluate` c)) $ derivative $ map toComplex p
