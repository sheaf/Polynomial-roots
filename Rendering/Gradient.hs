{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Rendering.Gradient where

import Control.Arrow
import Control.Applicative
import Data.Colour.Names
import Rendering.Colour
import Util

newtype Gradient clr a = Grad { runGrad :: Maybe Double -> clr a }

colourAt :: Gradient AlphaColour Double -> Maybe Double -> RGBAColour
colourAt = runGrad

apGrad :: Maybe Double -> Gradient f a -> f a
apGrad = flip runGrad

instance (AffineSpace f) => AffineSpace (Gradient f) where
    affineCombo xs z = Grad $ \n -> calcGrad n
      where calcGrad n = affineCombo (second (apGrad n) <$> xs) (apGrad n z)

instance (ColourOps f) => ColourOps (Gradient f) where
    over c (Grad f) = Grad $ \n -> over c (f n)
    darken s (Grad f) = Grad $ \n -> darken s (f n)

instance (Monoid (f a)) => Monoid (Gradient f a) where
    mempty = constant unit
    mappend (Grad g1) (Grad g2) = Grad $ \n -> g1 n <> g2 n


constant :: f a -> Gradient f a
constant = Grad . const

linear :: (Monoid (f a), Fractional a, AffineSpace f) => f a -> f a -> Gradient f a
linear c1 c2 = Grad $ maybe unit (\n -> blend (realToFrac n) c1 c2)

onInput :: (Double -> Double) -> Gradient f a -> Gradient f a
onInput f = Grad . (. fmap f) . runGrad

onOutput :: (f a -> g a) -> Gradient f a -> Gradient g a
onOutput f = Grad . (f .) . runGrad

invert, square, squareRoot :: Gradient f a -> Gradient f a
invert = onInput recip
square = onInput (^ 2)
squareRoot = onInput sqrt

adjacent :: Double -> f a -> Gradient f a -> Gradient f a -> Gradient f a
adjacent x z g1 g2 = Grad $ maybe z (\n -> runGrad (if n < x then g1 else g2) $ Just n)

asHue :: Gradient AlphaColour Double
asHue = Grad $ maybe transparent (\n -> opaque $ hsv n 1 1)

opacify :: (Num a) => Colour a -> Gradient AlphaColour a -> Gradient Colour a
opacify bg = onOutput (`over` bg)

fadeIn c = linear (opaque c) transparent
fadeOut c = linear transparent (opaque c)

warm = mconcat [square . square $ fadeIn white, square $ fadeIn yellow, fadeIn red]
cold = mconcat [square . square $ fadeIn white, square $ fadeIn cyan, fadeIn blue]

monochrome = fadeIn white

gradientByName "warm" = Just warm
gradientByName "cold" = Just cold
gradientByName "monochrome" = Just cold
gradientByName _ = Nothing




