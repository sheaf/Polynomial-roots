module AltPrelude ( module AltPrelude
                  , module Util
                  , module Prelude
                  , module Control.Category
                  , module Data.Foldable
                  , module Data.Traversable
                  ) where

-- generalized functions
import Control.Category

-- generalized lists
import Data.Foldable hiding (foldl1, foldr1)
import Data.Traversable

-- import the rest, hiding stuff we're replacing
import Prelude hiding ( (.), id
                      , foldr, foldr1, foldl, foldl1
                      , elem, notElem
                      , sum, product, and, or, all, any, maximum, minimum
                      , concat, concatMap
                      , mapM, mapM_, sequence, sequence_
                      
                      -- also hide terrible functions I don't want to look at
                      -- see below for corrected versions
                      , head, tail, last, init
                      , scanl1, scanr1
                      )

-- plus the misc. stuff here
import Util 

head []    = Nothing
head (x:_) = Just x

tail []     = Nothing
tail (_:xs) = Just xs

last []     = Nothing
last [x]    = Just x
last (x:xs) = last xs

init []     = Nothing
init [x]    = Just []
init (x:xs) = (x:) <$> init xs


