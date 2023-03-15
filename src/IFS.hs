module IFS where

-- base
import Control.Arrow
  ( (&&&) )
import Data.Complex

-- containers
import Data.Tree
  ( Forest )

-- polynomial-roots
import Interval
  ( Interval((+!)) )
import Polynomials
import Roots
  ( canHaveRoots )
import Types

--------------------------------------------------------------------------------
-- IFS plotting.

-- | Scales a point depending on the polynomial which produced it.
-- This makes the set of roots of a family of polynomials around a point
-- resemble the corresponding family of scaled values around that point.
scalePoint :: (Coefficient a) => Complex Double -> Scaler a
scalePoint c z p = z * scale
    where scale = (negate . recip . (`evaluate` c)) . derivative . map toComplex $ p

-- | Takes in a forest of polynomials and evaluates them at a point.
-- This produces a forest of points of the associated IFS.
ifsCounts :: (Coefficient a) => ((Polynomial a -> Root) -> (Polynomial a -> b))
          -> Config c a -> Scaler a -> Forest (Polynomial a) -> Forest b
ifsCounts q (Config _ _ _ c _ _ _) f forest = rootForest
    where eval       = flip evaluate c . map toComplex
          rootForest = (map . fmap) (q $ uncurry f . (eval &&& id)) forest

-- | Returns the forest of points of an IFS corresponding to the configuration.
ifsPoints :: (Coefficient a) => ((Polynomial a -> Root) -> (Polynomial a -> b))
          -> Config c a -> Forest b
ifsPoints q cfg@(Config ic (rx,ry) d c w s _g) = ifsForest
  where h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
        cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
        forest = canHaveRoots ic d cI
        ifsForest = ifsCounts q cfg scaling forest
        scaling = case s of
                       Left False -> \z _p -> z
                       Left True  -> scalePoint c
                       Right f    -> \z p -> (1-f') * z + f' * scalePoint c z p
                           where f' = toComplex f
