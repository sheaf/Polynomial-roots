module Rendering.ArrayRaster where

-- base
import Control.Applicative
import Control.Arrow ( second )
import Control.Monad
  ( when )
import Data.IORef

-- array
import Data.Array.MArray
import Data.Array.IO (IOArray)

-- polynomial-roots
import Interval (elemI)
import Rendering.Coord
import Rendering.Raster

--------------------------------------------------------------------------------

data IOArrayRaster val inp out where
    IOArrayRaster :: (Monoid v, MArray IOArray v IO)
                  => (i -> (InpCoord, v))
                  -> (v -> o)
                  -> (RstBounds, RstSize)
                  -> (InpBounds, InpSize)
                  -> IORef Integer
                  -> IOArray RstCoord v
                  -> IOArrayRaster v i o

instance Rasterizer IOArrayRaster where
    type RstContext IOArrayRaster = IO
    mkRasterizer i rb ib = mkRst <$> newIORef 0 <*> newArray rb mempty
      where mkRst = IOArrayRaster i id (mkBdSz rb) (mkBdSz ib)
    rasterize (IOArrayRaster f g rbs ibs n ar) i | validPoint = Just <$> updArray
                                                 | otherwise  = return Nothing
      where updArray = do
              v <- readArray ar xy
              let v' = iv <> v
              writeArray ar xy v'
              modifyIORef n (+ 1)
              count <- readIORef n
              when (count == 1) $
                putStrLn "Beginning to plot points."
              when (mod count 400000 == 0) $
                putStrLn (show count ++ " points plotted...")
              return (xy, g v')
            (ixy, iv) = f i
            ibC = (coordToComplex ib1, coordToComplex ib2)
            iC = coordToComplex ixy
            validPoint = iC `elemI` ibC && inRange rb xy
            ((ib1, ib2), ibSize) = ibs
            (rb, rbSize) = rbs
            rbSize' = fmap fromIntegral $ rbSize - mkCd2 1 1
            ratio = liftA2 (/) rbSize' ibSize
            xy = round <$> liftA2 (*) (ixy - ib1) ratio
    clear (IOArrayRaster _ _ _ _ _ ar) = clearArray mempty ar
    readOutput (IOArrayRaster _ o _ _ _ ar) = map (second o) <$> getAssocs ar
    withOutput (IOArrayRaster _ o _ _ _ ar) f = do ixs <- range <$> getBounds ar
                                                   mapM withElem ixs
      where withElem ix = f ix . o =<< readArray ar ix
    mapOutput f (IOArrayRaster i o rbs ibs n ar) = IOArrayRaster i (f.o) rbs ibs n ar
    mapInput :: (i' -> i) -> IOArrayRaster v i o -> IOArrayRaster v i' o
    mapInput f (IOArrayRaster i o rbs ibs n ar) = IOArrayRaster (i.f) o rbs ibs n ar
    inputBound (IOArrayRaster _ _ _ (ibs, _) _ _) = ibs
    outputBound (IOArrayRaster _ _ (rbs, _) _ _ _) = rbs

instance Functor (IOArrayRaster v i) where
    fmap = mapOutput

mkBdSz :: Num b => (b, b) -> ((b, b), b)
mkBdSz cd@(cd1, cd2) = (cd, cd2 - cd1)

clearArray :: (MArray a1 b m, Ix a2) => b -> a1 a2 b -> m ()
clearArray z ar = do bds <- getBounds ar
                     mapM_ (flip (writeArray ar) z) (range bds)
