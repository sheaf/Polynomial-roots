module Rendering.Raster where

-- base
import Control.Monad
  ( void )
import Data.Kind
  ( Type )

-- polynomial-roots
import Rendering.Coord

--------------------------------------------------------------------------------

class (Applicative (RstContext r), Monad (RstContext r)) => Rasterizer r where
    type RstContext r :: Type -> Type
    mkRasterizer :: (Monoid v) => (i -> (InpCoord, v))
                               -> RstBounds -> InpBounds
                               -> RstContext r (r v i v)
    rasterize :: r v i o -> i -> RstContext r (Maybe (RstCoord, o))
    rasterize_ :: r v i o -> i -> RstContext r ()
    clear :: r v i o -> RstContext r ()
    readOutput :: r v i o -> RstContext r [(RstCoord, o)]
    withOutput :: r v i o -> (RstCoord -> o -> RstContext r a) -> RstContext r [a]
    withOutput_ :: r v i o -> (RstCoord -> o -> RstContext r a) -> RstContext r ()
    mapOutput :: (o -> o') -> r v i o -> r v i o'
    mapInput :: (i' -> i) -> r v i o -> r v i' o
    inputBound :: r v i o -> InpBounds
    outputBound :: r v i o -> RstBounds
    rasterize_ r x = void $ rasterize r x
    withOutput_ r f = void $ withOutput r f

outputSize :: (Rasterizer r) => r v i o -> RstSize
outputSize = uncurry subtract . outputBound
