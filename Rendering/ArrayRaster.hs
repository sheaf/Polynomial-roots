{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Rendering.ArrayRaster where 

import Control.Arrow
import Data.Array.MArray
import Data.Array.IO
import Data.Monoid
import Rendering.Raster
import Rendering.Coord
import Pair
import Interval
import Types hiding (Config(..))
import Util

data IOArrayRaster val inp out where
    IOArrayRaster :: (Monoid v, MArray IOArray v IO) 
                  => (i -> (InpCoord, v)) 
                  -> (v -> o)
                  -> (RstBounds, RstSize)
                  -> (InpBounds, InpSize)
                  -> IOArray RstCoord v
                  -> IOArrayRaster v i o

instance Rasterizer IOArrayRaster where
    type RstContext IOArrayRaster = IO
    mkRasterizer i rb ib = do mkRst <$> newArray rb unit
      where mkRst = IOArrayRaster i id (mkBdSz rb) (mkBdSz ib)
    rasterize (IOArrayRaster f g rbs ibs ar) i | validPoint = Just <$> updArray 
                                               | otherwise  = do -- print ibSize
                                                                 return Nothing
      where updArray = do v <- readArray ar xy
                          let v' = iv <> v
                          writeArray ar xy v'
                          return (xy, g v')
            (ixy, iv) = f i
            ibC = (coordToComplex ib1, coordToComplex ib2)
            iC = coordToComplex ixy
            validPoint = iC `elemI` ibC && inRange rb xy
            (ib@(ib1, ib2), ibSize) = ibs
            (rb, rbSize) = rbs
            rbSize' = fmap fromIntegral $ rbSize - mkCd2 1 1
            ratio = liftA2 (/) rbSize' ibSize
            xy = round <$> liftA2 (*) (ixy - ib1) ratio
    clear (IOArrayRaster _ _ _ _ ar) = clearArray unit ar
    readOutput (IOArrayRaster _ o _ _ ar) = map (second o) <$> getAssocs ar
    withOutput (IOArrayRaster _ o _ _ ar) f = do ixs <- range <$> getBounds ar
                                                 mapM withElem ixs
      where withElem ix = f ix . o =<< readArray ar ix
    mapOutput f (IOArrayRaster i o rbs ibs ar) = IOArrayRaster i (f.o) rbs ibs ar
    mapInput f (IOArrayRaster i o rbs ibs ar) = IOArrayRaster (i.f) o rbs ibs ar
    inputBound (IOArrayRaster _ _ _ (ibs, _) _) = ibs
    outputBound (IOArrayRaster _ _ (rbs, _) _ _) = rbs

instance Functor (IOArrayRaster v i) where
    fmap = mapOutput

mkBdSz cd@(cd1, cd2) = (cd, cd2 - cd1)

clearArray z ar = do bds <- getBounds ar
                     mapM_ (flip (writeArray ar) z) (range bds)

testInput (IOArrayRaster i o rbs ibs ar) x = i x

