{-# LANGUAGE TypeFamilies #-}
module Image where

import Overture
import Prelude ()
import Control.Monad.IO.Class
import Data.Colour.Names
import Data.Foldable
import qualified Graphics.GD.ByteString.Lazy as GD
import Pair
import Settings
import Rendering.Colour
import Rendering.Raster
import Rendering.ArrayRaster
import Rendering.Coord
import Types hiding (outputSize)
import Rendering.Gradient

--------------------------------------------------------------------------------
--Image writing.

writePixel :: (ColourScheme c, m ~ ColourData c) => 
              GD.Image -> c -> RstCoord -> m -> IO()
writePixel img c xy v = GD.setPixel (toTuple xy) clr img
  where clr = fromColour $ flip over bg $ (toColour c) v
        bg  = flip over white $ (toColour c) mempty

writePixels :: (Rasterizer r, RstContext r ~ IO, ColourScheme c, m ~ ColourData c) 
            => r v i m -> GD.Image -> [i] -> c -> IO()
writePixels rst img [] c = return ()
writePixels rst img (p:ps) c = do px <- rasterize rst p
                                  whenJust (uncurry $ writePixel img c) px
                                  writePixels rst img ps c

writeImage :: (Foldable f, Rasterizer r, ColourScheme c, m ~ ColourData c, RstContext r ~ IO) 
           => f i -> r v i m -> c -> FilePath -> IO ()
writeImage xs rst c file = do
    let (rx,ry) = toTuple $ outputSize rst
    let bg = fromColour $ flip over white $ (toColour c) mempty
    image <- GD.newImage (rx, ry)
    GD.fillImage bg image
    writePixels rst image (toList xs) c
    GD.savePngFile file image

fromRGB8 (r,g,b) = GD.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
fromColour = fromRGB8 . toRGB8

dumpImage :: (Rasterizer r, ColourScheme c, m ~ ColourData c, RstContext r ~ IO) 
          => r v i m -> c -> FilePath -> IO ()
dumpImage rst c file = do image <- GD.newImage (rx, ry)
                          let bg = fromColour $ flip over white $ (toColour c) mempty
                          GD.fillImage bg image
                          withOutput_ rst (writePixel image c)
                          GD.savePngFile file image
  where (rx,ry) = toTuple $ outputSize rst
