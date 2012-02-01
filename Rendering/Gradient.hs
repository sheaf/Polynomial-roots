{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Rendering.Gradient where

import Overture
import Prelude ()
import Data.Colour.Names
import Data.Maybe
import Data.Monoid
import Rendering.Colour
import Data.List(lookup)
import Types

--Useful monoid amenable to colouring (for source colouring especially).
--Think: (R,G,B,Opacity).
--TODO: use standard colour types that are already defined!!
newtype SourceSum = Source (Double,Double,Double,Double)
instance Monoid SourceSum where
    mempty = Source (0,0,0,0) -- technically any (r,g,b,0)...
    mappend (Source (r1,g1,b1,n)) (Source (r2,g2,b2,m)) = 
        Source ( (r1*n + r2*m)/(n+m),(g1*n + g2*m)/(n+m),(b1*n + b2*m)/(n+m), n+m-n*m) 

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
onInput f g = Grad { runGrad =  (runGrad g) . f }

onOutput :: (f a -> g a) -> Gradient m f a -> Gradient m g a
onOutput f = Grad . (f .) . runGrad

invert, square, squareRoot :: Gradient Double f a -> Gradient Double f a
invert = onInput recip
square = onInput (^ 2)
squareRoot = onInput sqrt

adjacent :: Ord m => m -> Gradient m f a -> Gradient m f a -> Gradient m f a
adjacent x g1 g2 = Grad (\n -> runGrad (if n < x then g1 else g2) $ n)

--Takes a list of colours and control points, giving the corresponding gradient.
collate :: (Monoid (f a), Ord a, Fractional a, AffineSpace f) => [(f a, a)] -> Gradient a f a
collate cvs1 = Grad $ (blender cvs')
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

asHue :: Gradient Double AlphaColour Double
asHue = Grad (\n -> opaque $ hsv n 1 1)

opacify :: (Num a) => Colour a -> Gradient m AlphaColour a -> Gradient m Colour a
opacify bg = onOutput (`over` bg)

fadeIn c = linear (opaque c) transparent
fadeOut c = linear transparent (opaque c)

warm', cold', sunset' :: (Ord a, Floating a) => Gradient a AlphaColour a
warm' = collate [(opaque black,0),(opaque red,1/3),(opaque yellow,2/3),(opaque white,1)]
cold' = collate [(opaque black,0),(opaque blue,1/3),(opaque cyan,2/3),(opaque white,1)]
sunset' = collate [(opaque black,0),(opaque purple, 1/5), (opaque firebrick, 2/5), (opaque goldenrod, 1/2), (opaque orange, 3/5), (opaque white, 1)]

warm, cold, sunset :: (Ord a, Floating a) => Gradient (Sum a) AlphaColour a
warm = Grad { runGrad = (runGrad(warm') . getSum ) }
cold = Grad { runGrad = (runGrad(cold') . getSum ) }
sunset = Grad { runGrad = (runGrad(sunset') . getSum ) }

sourceGradient :: Gradient (SourceSum) AlphaColour Double
sourceGradient = Grad $ (\(Source (r,g,b,a)) -> rgba r g b a)

monochrome' = constant (opaque white)
monochrome = Grad { runGrad = (runGrad(monochrome') . getSum) }

--gradientByName :: String -> Maybe(Gradient Double AlphaColour Double)
gradientByName "warm" = Just warm
gradientByName "cold" = Just cold
gradientByName "sunset" = Just sunset
gradientByName "monochrome" = Just monochrome
gradientByName "transparent" = Just $ constant transparent
--gradientByName "source" = Just sourceGradient
gradientByName _ = Nothing


--gradientFromSpec :: Gradient m AlphaColour Double -> Colour Double 
--                 -> GradientSpec -> Gradient m Colour Double 
gradientFromSpec def bg gSpec = opacify bg $ fromExpr gSpec ?? def

fromExpr (NamedGradient g) = gradientByName g
fromExpr (Split gns) = splitGrads gns
fromExpr (Combine f xs) = combineGrads (getBlendFunc f) =<< mapM fromExpr xs
--fromExpr (Transform f g) = getTransFunc f <$> fromExpr g
fromExpr (Collate cols) = Just $ (onInput getSum) (collate cols')
    where cols' = (\(a,b) -> (a, getSum b)) <$> cols

-- combineGrads :: (Gradient AlphaColour a -> Gradient AlphaColour a -> Gradient AlphaColour a) 
--             -> [Gradient AlphaColour a] -> Maybe (Gradient AlphaColour a)
combineGrads f [] = Nothing
combineGrads f [x] = Just x
combineGrads f (x:xs) = Just $ foldl f x xs

splitGrads [] = Just $ constant mempty
splitGrads ((g1, n1):gns) = adjacent n1 <$> g1' <*> splitGrads gns
  where g1' = fromExpr g1

-- getBlendFunc :: BlendFunction -> Gradient AlphaColour Double 
--              -> Gradient AlphaColour Double -> Gradient AlphaColour Double
getBlendFunc Blend = blend 0.5
getBlendFunc Overlay = (++)

--getTransFunc :: (Monoid m, Monoid n) => (m -> n) 
--getTransFunc Invert = invert
--getTransFunc (Exponent n) = onInput (** n)
--getTransFunc Reverse = onInput (1 -)

--------------------------------------------------------------------------------
--Converting polynomials to values to be able to apply gradients.

--Converts a polynomials with coefficients in [c1,c2,...] to a polynomial
--with coefficients in [d1,d2,...].
--Somewhat dodgy, as it depends on exact matching of coefficients...
convertCoeffs :: (Coefficient a, Coefficient b) => [a] -> [b] -> Polynomial a -> Polynomial b 
convertCoeffs _ _ [] = []
convertCoeffs cfs dfs (c:cs) = case lookup c cfs' of
                                    Nothing -> error "couldn't look up coeff; possible rounding error"
                                    Just cf' -> cf':(convertCoeffs cfs dfs cs)
    where cfs' = zipWith (\x y -> (y,x)) dfs cfs

--Note: colouring depends on the order of the coefficients.
--This is a "base d" expansion.
toGValue1 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Double
toGValue1 cfs p = z * evaluate p' z
    where d = fromIntegral (length cfs) -1
          p' = convertCoeffs cfs [0..d] p
          z =  1 / fromIntegral (length cfs)

--This one should be used with gradients such that g(0)=g(1).
--This uses the scale factors.
toGValue2 :: (Coefficient a) => IterCoeffs a ->  Polynomial a -> Center -> Double
toGValue2 cfs p c = 0.5 + 1/(2*pi) * phase scale
    where scale = (negate . recip . (`evaluate` c)) $ derivative $ (map toComplex) $ p

