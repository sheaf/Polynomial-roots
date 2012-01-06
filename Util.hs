{-# LANGUAGE TypeOperators #-}
module Util ( module Util
            , module Control.Category 
            , module Control.Arrow 
            , module Control.Applicative 
            , module Control.Monad
            , module Data.Functor
            , module Data.Maybe
            , module Data.List
            , module Data.Monoid
            , module Data.Complex
            , module Data.Label.Pure
            ) where

import Control.Category (Category(), (>>>), (<<<))
import Control.Arrow ( first, second, (***), (&&&)
                     , left, right, (+++), (|||)
                     )
import Control.Applicative (Applicative(..), liftA2, liftA3)
import Control.Monad ( (=<<), (<=<), (>=>), join
                     , liftM, liftM2, liftM3, liftM4
                     , guard, unless, when
                     , replicateM, void
                     )
import Data.Functor ((<$>))
import Data.Foldable (Foldable(), toList, traverse_, sequenceA_)
import Data.Traversable (Traversable(), traverse, sequenceA)
import Data.Maybe (maybe, fromMaybe, catMaybes, mapMaybe)
import Data.List (unfoldr)
import Data.Monoid (Monoid(..))
import Data.Complex
import Data.Label.Pure

complexToPair :: Complex a -> (a, a)
complexToPair (r :+ i) = (r, i)

(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

unit :: (Monoid a) => a
unit = mempty

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

takeUntilDiff :: (a -> a -> Bool) -> [a] -> [a]
takeUntilDiff f (x1:x2:xs) | f x1 x2   = [x1]
                           | otherwise = x1:takeUntilDiff f (x2:xs)
takeUntilDiff _ xs = xs

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x | p x 	   = Just x
           | otherwise = Nothing


whenJust :: (a -> IO b) -> Maybe a -> IO ()
whenJust _ Nothing = return ()
whenJust f (Just x) = f x >> return ()


untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = untilM p f =<< f x

unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f b = do x <- f b
                 case x of Just (a, b') -> liftM (a:) (unfoldM f b)
                           Nothing      -> return []

unfoldActionM :: (Monad m) => m (Maybe a) -> m [a]
unfoldActionM mx = step =<< mx
  where step (Just x) = liftM (x:) (unfoldActionM mx)
        step Nothing  = return []

