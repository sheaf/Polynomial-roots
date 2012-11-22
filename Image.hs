{-# LANGUAGE TypeFamilies #-}
module Image where

import Overture
import Prelude ()
import Data.Colour.Names
import qualified Graphics.GD.ByteString.Lazy as GD

import Pair (Pair(toTuple))
import Rendering.Colour (ColourOps(over), toRGB8)
import Rendering.Raster
import Rendering.Coord (RstCoord)
import Types hiding (outputSize)

--------------------------------------------------------------------------------
--Image writing.

writePixel :: ColourScheme c 
           => GD.Image -> c -> RstCoord -> ColourData c -> IO()
writePixel img c xy v = GD.setPixel (toTuple xy) clr img
  where clr = fromColour $ flip over bgc $ (toColour c) v
        bgc = flip over black $ bg c

writePixels :: (Rasterizer r, RstContext r ~ IO, ColourScheme c) 
            => r v i (ColourData c) -> GD.Image -> [i] -> c -> IO()
writePixels rst img [] c = return ()
writePixels rst img (p:ps) c = do px <- rasterize rst p
                                  whenJust (uncurry $ writePixel img c) px
                                  writePixels rst img ps c

writeImage :: (Foldable f, Rasterizer r, ColourScheme c, RstContext r ~ IO) 
           => f i -> r v i (ColourData c) -> c -> FilePath -> IO ()
writeImage xs rst c file = do
    let (rx,ry) = toTuple $ outputSize rst
    let bgc = fromColour $ flip over black $ bg c
    image <- GD.newImage (rx, ry)
    GD.fillImage bgc image
    writePixels rst image (toList xs) c
    GD.savePngFile file image

fromRGB8 (r,g,b) = GD.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
fromColour = fromRGB8 . toRGB8

dumpImage :: (Rasterizer r, ColourScheme c, m ~ ColourData c, RstContext r ~ IO) 
          => r v i m -> c -> FilePath -> IO ()
dumpImage rst c file = do image <- GD.newImage (rx, ry)
                          let bgc = fromColour $ flip over black $ bg c
                          GD.fillImage bgc image
                          withOutput_ rst (writePixel image c)
                          GD.savePngFile file image
  where (rx,ry) = toTuple $ outputSize rst
