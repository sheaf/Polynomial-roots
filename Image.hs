{-# LANGUAGE TypeFamilies #-}
module Image where

import Overture
import Prelude ()
import Control.Monad.IO.Class
import Data.Foldable
import qualified Graphics.GD.ByteString.Lazy as GD
import Pair
import Settings
import Rendering.Colour
import Rendering.Raster
import Rendering.ArrayRaster
import Rendering.Coord
import Types(Gradient(..))
import Rendering.Gradient

--------------------------------------------------------------------------------
--Image writing.

writePixel :: GD.Image -> Gradient m Colour Double -> RstCoord -> m -> IO()
writePixel img g xy c = GD.setPixel (toTuple xy) clr img
  where clr = fromColour $ runGrad g c

writePixels :: (Rasterizer r, RstContext r ~ IO) 
            => r v i m -> GD.Image 
            -> [i] -> Gradient m Colour Double -> IO()
writePixels rst img [] g = return ()
writePixels rst img (p:ps) g = do px <- rasterize rst p
                                  whenJust (uncurry $ writePixel img g) px
                                  writePixels rst img ps g


writeImage :: (Foldable f, Rasterizer r, Monoid m, RstContext r ~ IO) 
           => f i -> r v i m
           -> Gradient m Colour Double -> FilePath -> IO ()
writeImage xs rst g file = do
    let (rx,ry) = toTuple $ outputSize rst
    image <- GD.newImage (rx, ry)
    GD.fillImage (fromColour $ runGrad g mempty) image
    writePixels rst image (toList xs) g
    GD.savePngFile file image


fromRGB8 (r,g,b) = GD.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
fromColour = fromRGB8 . toRGB8

dumpImage :: (Rasterizer r, Monoid m, RstContext r ~ IO) 
          => r v i m -> Gradient m Colour Double -> FilePath -> IO ()
dumpImage rst g file = do image <- GD.newImage (rx, ry)
                          GD.fillImage (fromColour $ runGrad g mempty) image
                          withOutput_ rst (writePixel image g)
                          GD.savePngFile file image
  where (rx,ry) = toTuple $ outputSize rst
