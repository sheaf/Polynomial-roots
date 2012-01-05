module Rendering.Colour ( module Rendering.Colour
                        , module Data.Colour
                        ) where

import Data.Word
import Data.Colour
import Data.Colour.SRGB (sRGBSpace, toSRGB, toSRGB24)
import Data.Colour.RGBSpace (RGB(..), uncurryRGB, curryRGB, rgbUsingSpace)
import Data.Colour.RGBSpace.HSV (hsvView)
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSV as HSV

type RGBColour = Colour Double
type RGBAColour = AlphaColour Double

rgb :: Double -> Double -> Double -> RGBColour
rgb = SRGB.sRGB

rgba :: Double -> Double -> Double -> Double -> RGBAColour
rgba r g b = withOpacity (rgb r g b) 

toRGB :: RGBColour -> (Double, Double, Double)
toRGB = uncurryRGB (,,) . toSRGB

toRGB8 :: RGBColour -> (Word8, Word8, Word8)
toRGB8 = uncurryRGB (,,) . toSRGB24

hsv :: Double -> Double -> Double -> RGBColour
hsv h s v = toSRGBSpace $ HSV.hsv (clamp 1 h * 360) s v

toHSV :: RGBColour -> (Double, Double, Double)
toHSV = hsvView . toSRGB

toSRGBSpace :: RGB Double -> RGBColour
toSRGBSpace = uncurryRGB (rgbUsingSpace sRGBSpace)

-- this is a hack and may introduce errors...
clamp :: Double -> Double -> Double
clamp n x | x < 0     = 0
          | x < n     = x
          | otherwise = x - fromInteger (floor (x / n)) * n

