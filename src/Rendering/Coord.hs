module Rendering.Coord where

-- base
import Prelude
  hiding ( sum, elem, fst, snd, and, or )
import Control.Applicative
import Data.Complex
import Data.Foldable
  ( sum, and )
import Data.Ix
  ( Ix )
import Data.Kind
  ( Type )

-- polynomial-roots
import Pair

--------------------------------------------------------------------------------

type RstCoord = Cd2 Int
type RstSize = RstCoord
type RstBounds = (RstCoord, RstCoord)

type InpCoord = Cd2 Double
type InpSize = InpCoord
type InpBounds = (InpCoord, InpCoord)


newtype Cd2 a = Cd2 { getCd2 :: StrictPair a }
  deriving newtype
    ( Eq, Ord, Read, Show, Ix
    , Functor, Applicative, Foldable )
  deriving stock Traversable

mkCd2 :: a -> a -> Cd2 a
mkCd2 x y = Cd2 (StrictPair x y)

dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot c1 c2 = sum $ liftA2 (*) c1 c2

coordToComplex :: Cd2 a -> Complex a
coordToComplex = pair (:+)



instance Pair (Cd2 a) where
    type PairElem (Cd2 a) = a
    pair f = pair f . getCd2

instance (Num a) => Num (Cd2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*) -- this makes no sense but whatever
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger



class Scale a where
    type Scalar a :: Type
    (.*) :: Scalar a -> a -> a
    (*.) :: a -> Scalar a -> a

instance (Num a) => Scale (Complex a) where
    type Scalar (Complex a) = a
    n .* (r :+ i) = n * r :+ n * i
    (r :+ i) *. n = r * n :+ i * n

instance (Num a) => Scale (Cd2 a) where
    type Scalar (Cd2 a) = a
    n .* cd = (n *) <$> cd
    cd *. n = (* n) <$> cd




class Range a where
    type RangeElem a :: Type
    elem :: RangeElem a -> a -> Bool
    size :: a -> RangeElem a

data OrdSpan a = OrdSpan a a
    deriving stock (Eq, Ord, Read, Show)

instance Pair (OrdSpan a) where
    type PairElem (OrdSpan a) = a
    pair f (OrdSpan x y) = f x y

instance (Num a, Ord a) => Range (OrdSpan a) where
    type RangeElem (OrdSpan a) = a
    elem x sp = x > fst sp && x < snd sp
    size = pair subtract

instance (Num a, Ord a) => Range (Cd2 a, Cd2 a) where
    type RangeElem (Cd2 a, Cd2 a) = Cd2 a
    elem x (r1, r2) = and (liftA2 (<) x r1) && and (liftA2 (>) x r2)
    size (r1, r2) = r2 - r1





