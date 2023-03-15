{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Rendering.Gradient where

-- base
import Control.Arrow
  ( second )
import Data.Complex
  ( Complex, phase )
import Data.List
  ( sortBy )
import Data.Maybe
  ( fromJust )
import Data.Monoid
import Data.Ord
  ( comparing )

-- containers
import qualified Data.Map as Map
  ( lookup )

-- polynomial-roots
import Configuration
import Polynomials
import Rendering.Colour
import qualified Rendering.Colour.Names as Names
  ( gradientDict, gradientNames )
import Types

--------------------------------------------------------------------------------

apGrad :: m -> Gradient m f a -> f a
apGrad = flip runGrad

instance (AffineSpace f) => AffineSpace (Gradient m f) where
    affineCombo xs z = Grad $ \n -> calcGrad n
      where calcGrad n = affineCombo (second (apGrad n) <$> xs) (apGrad n z)

instance (ColourOps f) => ColourOps (Gradient m f) where
    over c (Grad f) = Grad $ \n -> over c (f n)
    darken s (Grad f) = Grad $ \n -> darken s (f n)

instance (Semigroup (f a)) => Semigroup (Gradient m f a) where
    (Grad g1) <> (Grad g2) = Grad $ \n -> (g1 n) <> (g2 n)

instance (Monoid (f a)) => Monoid (Gradient m f a) where
    mempty = constant mempty

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
reverseGrad = onInput ( \ x -> 1 - x )
invert = onInput recip
square = onInput ( ^ ( 2 :: Int ) )
squareRoot = onInput sqrt

adjacent :: Ord m => m -> Gradient m f a -> Gradient m f a -> Gradient m f a
adjacent x g1 g2 = Grad (\n -> runGrad (if n < x then g1 else g2) n)

--Takes a list of colours and control points, giving the corresponding gradient.
collate :: forall f a. (Monoid (f a), Ord a, Fractional a, AffineSpace f) => [(f a, a)] -> Gradient a f a
collate cvs1 = Grad $ blender cvs'
    where
      cvs2 = sortBy (comparing snd) cvs1
      cvs3 = filter (\(_,b) -> (b >= 0 && b <= 1)) cvs2
      cvs'
        | null cvs3
        = error "could not collate, too few control points..."
        | otherwise
        = case (head cvs3, last cvs3) of
            ((_,0),(_,1)) -> cvs3
            ((_,0),(b,_)) -> cvs3 ++ [(b,1)]
            ((a,_),(_,1)) -> [(a,0)] ++ cvs3
            ((a,_),(b,_)) -> [(a,0)] ++ cvs3 ++ [(b,1)]

      blender :: [(f a, a)] -> a -> f a
      blender cvs n = blend n' c2 c1 -- arguments inverted again!!
          where n2 = min 1 . max 0 $ n
                (c1,a1) = head $ dropWhile (\(_,b) -> b > n2) (reverse cvs)
                (c2,a2) = head $ dropWhile (\(_,b) -> b < n2) cvs
                n' = if a1==a2 then n2 else (n2-a1)/(a2-a1)

withOpacityG :: Num a => a -> Gradient a Colour a -> Gradient a AlphaColour a
withOpacityG o = onOutput (`withOpacity` o)

collateWithOp :: (Ord a, Fractional a) => a -> [(Colour a, a)] -> Gradient a AlphaColour a
collateWithOp o = withOpacityG o . collate

opacify :: (Num a) => Colour a -> Gradient m AlphaColour a -> Gradient m Colour a
opacify bg_col = onOutput (`over` bg_col)

-- fadeIn c = linear (opaque c) transparent
-- fadeOut c = linear transparent (opaque c)

monochrome :: Num a => Colour a -> a -> AlphaColour a
monochrome = withOpacity
monochrome' :: t1 -> t2 -> Gradient m clr a
monochrome' o c = Grad { runGrad = runGrad (monochrome' o c)}
hsvGrad' :: Double -> Double -> AlphaColour Double
hsvGrad' o d = flip withOpacity o $ hsv d 1 1
hsvGrad :: Double -> Gradient Double AlphaColour Double
hsvGrad o = Grad { runGrad = hsvGrad' o }

gradientByName' :: String -> (Double -> Gradient Double AlphaColour Double)
gradientByName' "hsv" = hsvGrad
gradientByName' s = case Map.lookup s Names.gradientDict of
                         Just g -> flip collateWithOp g
                         _      -> error $ "unrecognised gradient name: " ++ s

gradientNames :: [String]
gradientNames = "hsv" : Names.gradientNames

gradientByName :: (String, Maybe Double) -> Maybe (Gradient Double AlphaColour Double)
gradientByName (s,d) = case d of
                            Just d' -> Just $ gradientByName' s d'
                            Nothing -> Just $ gradientByName' s 1

--gradientFromSpec def bg gSpec
--  = opacify bg
--  $ fromExpr gSpec ?? def

fromExpr :: GradientSpec -> Maybe (Gradient Double AlphaColour Double)
fromExpr (NamedGradient g) = gradientByName g
fromExpr (Split gns) = splitGrads gns
fromExpr (Combine f xs) = combineGrads (getBlendFunc f) =<< mapM fromExpr xs
fromExpr (Collate cols) = Just $ collate cols

combineGrads :: (a -> a -> a) -> [a] -> Maybe a
combineGrads _ [] = Nothing
combineGrads _ [x] = Just x
combineGrads f (x:xs) = Just $ foldl f x xs

splitGrads :: [(GradientSpec, Double)] -> Maybe (Gradient Double AlphaColour Double)
splitGrads [] = Just $ constant mempty
splitGrads ((g1, n1):gns) = adjacent n1 <$> g1' <*> splitGrads gns
  where g1' = fromExpr g1

getBlendFunc :: (AffineSpace f, Fractional a, Semigroup (f a)) => BlendFunction -> f a -> f a -> f a
getBlendFunc Blend = blend 0.5
getBlendFunc Overlay = (<>)

newtype WeightedAlphaColour a = W (AlphaColour a, Int)
fromWeighted :: forall a
             .  (Eq a, Num a, Fractional a)
             => WeightedAlphaColour a -> AlphaColour a
fromWeighted (W(c,w)) = go c w
    where
      go :: AlphaColour a -> Int -> AlphaColour a
      go _   0 = mempty
      go col 1 = col
      go col n = case alphaChannel c of
                    0 -> mempty
                    a -> dissolve ((1 - (1-a)^n)/a) col

instance Fractional a => Semigroup (WeightedAlphaColour a) where
  (W(c1,w1)) <> (W(c2, w2)) = W (blend (v1 / (v1+v2)) c1 c2, w1+w2)
    where v1 = fromIntegral w1
          v2 = fromIntegral w2
instance Fractional a => Monoid (WeightedAlphaColour a) where
    mempty = W (transparent, 0)


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
convertCoeffs :: (Coefficient a, Coefficient b)
              => [a] -> [b] -> Polynomial a -> Polynomial b
convertCoeffs _ _ [] = []
convertCoeffs cfs dfs (c:cs) = cf : convertCoeffs cfs dfs cs
    where cfs' = zip cfs dfs
          cf'  = closest c cfs
          cf   = maybe (error "error looking up coefficient... rounding error?")
                       id $ lookup cf' cfs'

--Note: colouring depends on the order of the coefficients.
--This is a "base d" expansion.
toGValue1 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Double
toGValue1 cfs p = z * evaluate p' z
    where d  = fromIntegral (length cfs) - 1
          p' = convertCoeffs cfs [0..d] p
          z  = 1 / fromIntegral (length cfs)

--This one should be used with gradients such that g(0)=g(1).
--This uses the scale factors.
toGValue2 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Center -> Double
toGValue2 _cfs p c = 0.5 + 1/(2*pi) * phase scale
    where scale = (negate . recip . (`evaluate` c)) $ derivative $ map toComplex p
