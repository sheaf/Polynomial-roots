{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Pair where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid (mappend)
import Data.Ix (Ix)
import Data.Complex
import Prelude hiding (fst, snd)
import qualified Prelude as P

-- req'd def: pair
class Pair a where
    type PairElem a :: *
    pair      :: (PairElem a -> PairElem a -> r) -> a -> r
    fst       :: a -> PairElem a
    snd       :: a -> PairElem a
    toTuple   :: a -> (PairElem a, PairElem a)
    fst = pair const
    snd = pair (flip const)
    toTuple = pair (,)

instance Pair (a, a) where
    type PairElem (a, a) = a
    pair = uncurry
    fst = P.fst
    snd = P.snd
    toTuple = id

instance Pair (Complex a) where
    type PairElem (Complex a) = a
    pair f (r :+ i) = f r i



data StrictPair a = StrictPair { spFst :: !a, spSnd :: !a  }
    deriving (Eq, Ord, Read, Show, Ix)

instance Pair (StrictPair a) where
    type PairElem (StrictPair a) = a
    pair f (StrictPair x y) = f x y
    fst = spFst
    snd = spSnd

instance Functor StrictPair where
    fmap f (StrictPair x y) = StrictPair (f x) (f y)

instance Applicative StrictPair where
    pure n = StrictPair n n
    StrictPair f g <*> StrictPair x y = StrictPair (f x) (g y)

instance Foldable StrictPair where
    foldMap f = pair mappend . fmap f
    foldr f z (StrictPair x y) = f x (f y z)

instance Traversable StrictPair where
    traverse f = sequenceA . fmap f
    sequenceA = pair (liftA2 StrictPair)


data LazyPair a = LazyPair { lzFst :: a, lzSnd :: a }
    deriving (Eq, Ord, Read, Show, Ix)

instance Pair (LazyPair a) where
    type PairElem (LazyPair a) = a
    pair f (LazyPair x y) = f x y
    fst = lzFst
    snd = lzSnd

instance Functor LazyPair where
    fmap f (LazyPair x y) = LazyPair (f x) (f y)

instance Applicative LazyPair where
    pure n = LazyPair n n
    LazyPair f g <*> LazyPair x y = LazyPair (f x) (g y)

instance Foldable LazyPair where
    foldMap f = pair mappend . fmap f
    foldr f z (LazyPair x y) = f x (f y z)

instance Traversable LazyPair where
    traverse f = sequenceA . fmap f
    sequenceA = pair (liftA2 LazyPair)

