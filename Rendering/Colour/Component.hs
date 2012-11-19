module Rendering.Colour.Component where

import Data.Colour.SRGB (toSRGB)
import qualified Data.Colour.RGBSpace.HSV as HSV
import Rendering.Colour 

red, green, blue, hue, saturation, value :: RGBColour -> Double
red = channelRed . toSRGB
green = channelGreen . toSRGB
blue = channelBlue . toSRGB
hue = HSV.hue . toSRGB
saturation = HSV.saturation . toSRGB
value = HSV.value . toSRGB

