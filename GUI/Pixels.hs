module GUI.Pixels where

import Control.Applicative
import Control.Monad
import Foreign

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Plotting

withLock :: (Surface -> IO a) -> Surface -> IO a
withLock f surf = lockSurface surf *> f surf <* unlockSurface surf

type XY = (Int, Int)

mapPixel :: XY -> (RGB8 -> RGB8) -> Surface -> IO ()
mapPixel xy f surf = do rgb <- getPixel xy surf
                        setPixel xy (f rgb) surf

xyToOffset :: XY -> Surface -> Int
xyToOffset (x, y) surf = y * surfaceGetWidth surf + x

getPixel :: XY -> Surface -> IO RGB8
getPixel xy s = do pixels <- castPtr <$> surfaceGetPixels s
                   px <- Pixel <$> peekElemOff pixels (xyToOffset xy s)
                   getRGB px (surfaceGetPixelFormat s)

setPixel :: XY -> RGB8 -> Surface -> IO ()
setPixel xy (r,g,b) s = do Pixel px <- mapRGB (surfaceGetPixelFormat s) r g b
                           pxs <- castPtr <$> surfaceGetPixels s
                           pokeElemOff pxs (xyToOffset xy s) px

incPixel :: RGB8 -> RGB8
incPixel (r, g, b) = (incColor r, incColor g, incColor b)

incColor :: Word8 -> Word8
incColor x | x < 255   = x + 50
           | otherwise = 255

