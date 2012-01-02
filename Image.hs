module Image where

import qualified Graphics.GD.ByteString.Lazy as GD
import Types

--------------------------------------------------------------------------------
--Image writing.

writePixel:: GD.Image -> Pixel -> Gradient GD.Color -> IO()
writePixel image (px,py) (grad,_) = do
    col <- GD.getPixel (px,py) image
    let col' = grad col
    GD.setPixel (px,py) col' image

writePixels :: GD.Image -> [Pixel] -> Gradient GD.Color -> IO()
writePixels image p g
    | null p = return()
    | otherwise = do writePixel image (head p) g
                     writePixels image (tail p) g

writeImage :: FilePath -> [Pixel] -> Resolution -> Gradient GD.Color -> IO()
writeImage file pixels (rx,ry) (grad,s) = do
    let pixels' = map (\(px,py) -> (px,ry-py)) pixels 
    image <- GD.newImage (rx,ry)     -- ^^ makes sure y-coord is not flipped
    let col = 0 --change for other gradients!!
    GD.fillImage col image
    writePixels image pixels' (grad,s)
    GD.savePngFile file image
