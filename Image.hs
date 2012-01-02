module Image where

import qualified Graphics.GD.ByteString.Lazy as GD
import Types
import Util
import Rendering.Raster

--------------------------------------------------------------------------------
--Image writing.

--this possibly flips the image, check!!
writePixel:: Int -> GD.Image -> Gradient v GD.Color -> (RstCoord, v) -> IO()
writePixel ry image (grad,_) ((px,py), c) = do
    let col' = grad c
    GD.setPixel (px,ry-py) col' image

writePixels :: Int -> GD.Image -> [IO (Maybe (RstCoord, v))] 
            -> Gradient v GD.Color -> IO()
writePixels ry image [] g = return ()
writePixels ry image (p:ps) g = do whenJust (writePixel ry image g) =<< p
                                   writePixels ry image ps g

writeImage :: (Monoid v) => FilePath -> [IO (Maybe (RstCoord, v))] 
           -> Resolution -> Gradient v GD.Color -> IO()
writeImage file pixels (rx,ry) (grad,s) = do
    image <- GD.newImage (rx,ry)
    GD.fillImage (grad mempty) image
    writePixels ry image pixels (grad, s)
    GD.savePngFile file image
