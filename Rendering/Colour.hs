module Rendering.Colour ( module Rendering.Colour
                        , module Data.Colour
                        ) where

import Overture
import Prelude()

import Data.Word (Word8, Word32)
import Data.Colour
import Data.Colour.SRGB ( sRGBSpace, toSRGB, toSRGB24 )
import Data.Colour.RGBSpace ( RGB, rgbUsingSpace, uncurryRGB )
import Data.Colour.RGBSpace.HSV ( hsvView )
import qualified Data.Colour.SRGB as SRGB ( sRGB )
import qualified Data.Colour.RGBSpace.HSV as HSV ( hsv )
import Data.Maybe (fromJust)

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

rgbaToWord32 :: RGBAColour -> Word32
rgbaToWord32 acol = (\(r,g,b) -> 0x010000 * fromIntegral r 
                               + 0x000100 * fromIntegral g 
                               + 0x000001 * fromIntegral b) (toRGB8 col)
                  + 0x0100000000 * (floor $ alphaChannel acol)
    where col = acol `over` black

colourFromHex :: String -> RGBColour
colourFromHex hex = SRGB.sRGB r g b
    where r' = take 2 hex
          g' = take 2 (drop 2 hex)
          b' = take 2 (drop 4 hex)
          [r,g,b] = map go [r',g',b']
              where go x = (/255) . fromJust . read $ "0x" ++ x
                            
aColourFromHex :: String -> String -> RGBAColour
aColourFromHex hex alpha = colourFromHex hex `withOpacity` a
    where a = (/255) . fromJust . read $ "0x" ++ alpha

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

