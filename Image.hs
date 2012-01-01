module Image where

import Foreign.C.Types(CInt)
import qualified Graphics.GD.ByteString.Lazy as GD
import Types

--------------------------------------------------------------------------------
--Image writing.

toInt :: Colour -> CInt
toInt [r,g,b] = b' + 256*g' + (256^2)*r'
    where [r',g',b'] = map fromIntegral [r,g,b]    
toInt _ = 0

--this possibly flips the image, check!!
writePixel:: GD.Image -> Pixel -> Gradient -> IO()
writePixel image (px,py) (grad,_) = do
    --make this non-monochrome!!
    let col = toInt (grad 1)
    GD.setPixel (px,py) col image

writePixels :: GD.Image -> [Pixel] -> Gradient -> IO()
writePixels image p g
    | null p = return()
    | otherwise = do writePixel image (head p) g
                     writePixels image (tail p) g

writeImage :: FilePath -> [Pixel] -> Resolution -> Gradient -> IO()
writeImage file pixels (rx,ry) (grad,s) = do
    image <- GD.newImage (rx,ry)
    let col = toInt (grad 0)
    GD.fillImage col image
    writePixels image pixels (grad,s)
    GD.savePngFile file image
