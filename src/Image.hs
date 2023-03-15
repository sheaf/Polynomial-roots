module Image where

-- base
import Data.Foldable
  ( for_, toList )
import GHC.Exts
  ( RealWorld )

-- JuicyPixels
import qualified Codec.Picture as JuicyPixels
import qualified Codec.Picture.Types as JuicyPixels

-- polynomial-roots
import Pair
  ( Pair(toTuple) )
import Rendering.Colour
  ( toRGBA )
import Rendering.Raster
import Rendering.Coord
  ( RstCoord )
import Types ( ColourScheme(bg, ColourData, toColour) )

--------------------------------------------------------------------------------
--Image writing.

type Image = JuicyPixels.MutableImage RealWorld JuicyPixels.PixelRGBA8

writePixel :: ColourScheme c
           => Image -> c -> RstCoord -> ColourData c -> IO()
writePixel img c xy v = JuicyPixels.writePixel img x y clr
  where (x,y) = toTuple xy
        clr = toRGBA JuicyPixels.PixelRGBA8 $ toColour c v

writePixels :: (Rasterizer r, RstContext r ~ IO, ColourScheme c)
            => r v i (ColourData c) -> Image -> [i] -> c -> IO()
writePixels _   _   []     _ = return ()
writePixels rst img (p:ps) c =
  do px <- rasterize rst p
     for_ px (uncurry $ writePixel img c)
     writePixels rst img ps c

writeImage :: (Foldable f, Rasterizer r, ColourScheme c, RstContext r ~ IO)
           => f i -> r v i (ColourData c) -> c -> FilePath -> IO ()
writeImage xs rst c =
  withImage ( \ image -> writePixels rst image ( toList xs ) c ) rst c

dumpImage :: (Rasterizer r, ColourScheme c, m ~ ColourData c, RstContext r ~ IO)
          => r v i m -> c -> FilePath -> IO ()
dumpImage rst c =
  withImage ( \ image -> withOutput_ rst ( writePixel image c ) ) rst c

withImage :: (Rasterizer r, ColourScheme c)
          => ( Image -> IO () )
          -> r v i m -> c -> FilePath -> IO ()
withImage f rst c file = do
  image <- JuicyPixels.newMutableImage rx ry
  let bgc = toRGBA JuicyPixels.PixelRGBA8 $ bg c
  JuicyPixels.fillImageWith image bgc
  f image
  immage <- JuicyPixels.unsafeFreezeImage $ image
  JuicyPixels.savePngImage file (JuicyPixels.ImageRGBA8 immage)
  where (rx,ry) = toTuple $ outputSize rst
