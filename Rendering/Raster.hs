{-# LANGUAGE GADTs #-}
module Rendering.Raster where 

import Control.Arrow
import Data.Array.MArray
import Data.Array.IO
import Data.Monoid
import Interval
import Types
import Util


type RstCoord = (Int, Int)
type RstBounds = (RstCoord, RstCoord)

type InpCoord = (Double, Double)
type InpBounds = (InpCoord, InpCoord)

type RasterizerIO = Rasterizer IO IOArray

data Rasterizer m ar inp val where
    Rasterizer :: (Monoid v, Applicative m, Monad m, MArray a v m) 
               => (i -> (InpCoord, v)) 
               -> RstBounds
               -> (InpBounds, InpCoord)
               -> a RstCoord v
               -> Rasterizer m a i v


mkRasterizer :: (Monoid v, Applicative m, Monad m, MArray a v m) 
             => (i -> (InpCoord, v)) 
             -> RstCoord -> InpBounds
             -> m (Rasterizer m a i v)
mkRasterizer f rb ib = do array <- newArray rb' unit
                          return $ mkRst array
  where rb' = ((0,0), rb)
        mkRst = Rasterizer f rb' (ib, getWH ib)
        getWH ((x1,y1), (x2,y2)) = (x2-x1, y2-y1)

getInpBounds (Rasterizer _ _ (ib, _) _) = ib

-- TODO: this is ugly and needs to be improved now that things work
rasterize :: Rasterizer m a i v -> i -> m (Maybe (RstCoord, v))
rasterize (Rasterizer f rb ibs ar) i | validPoint = Just <$> updateArray 
                                     | otherwise  = return Nothing
  where updateArray = do v <- readArray ar (x,y)
                         let v' = iv <> v
                         writeArray ar (x,y) v'
                         return ((x,y), v')
        validPoint = (ix :+ iy) `elemI` ibC && inRange rb (x, y)
        ((ix, iy), iv) = f i
        (ib@((ibx1, iby1), (ibx2, iby2)), (ibw, ibh)) = ibs
        ibC = (ibx1 :+ iby1, ibx2 :+ iby2)
        (_, (rbw, rbh)) = rb
        x = round $ ((ix - ibx1) / ibw) * fromIntegral (rbw - 1)
        y = round $ ((iy - iby1) / ibh) * fromIntegral (rbh - 1)

createRasterizerIO :: (Monoid v) => (i -> (InpCoord, v)) 
                   -> Config v c a -> IO (RasterizerIO i v)
createRasterizerIO f cfg = mkRasterizer f rb ib
  where rb@(rx,ry) = resolution cfg
        [rx', ry'] = fromIntegral <$> [rx, ry]
        w = width cfg
        c = center cfg
        h = w * ry'/rx'
        szC = w/2 :+ h/2
        ib = (complexToPair $ c - szC, complexToPair $ c + szC)


