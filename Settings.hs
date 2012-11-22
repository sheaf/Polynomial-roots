{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Settings ( module Settings
                , module Data.Complex
                , module Data.Label.Pure
                , module Control.Applicative
                , module Control.Category
                , module Control.Monad.Reader
                ) where

import Control.Monad.Reader
import Control.Monad.Identity (Identity)
import Control.Applicative
import Control.Category (Category(), (>>>), (<<<))
import Data.Complex
import Data.Label (mkLabels)
import Data.Label.Pure

import Types (outputSize, render, windowCenter, windowSize
             , Width, Center, Resolution)
import qualified Types as T ( Config(Config) )
import qualified Pair as P ( StrictPair(spFst), Pair(toTuple) )
import Rendering.Coord ( Cd2(getCd2) )

type Height = Double

data Settings = Settings { _resolution :: Resolution
                         , _center     :: Center
                         , _width      :: Width
                         }

$(mkLabels [''Settings])

aspect :: Settings -> Double
aspect st = let (rx,ry) = get resolution st
            in fromIntegral rx / fromIntegral ry

height :: Settings :-> Height
height = lens (\s -> get width s / aspect s)
              (\h s -> set width (h * aspect s) s)

size :: Settings -> (Width, Height)
size s = (get width s, get height s)

newtype EnvT m a = Env { unwrapEnv :: ReaderT Settings m a }
    deriving ( Functor, Applicative, Monad
             , MonadReader Settings, MonadIO
             )

type Env = EnvT Identity
type EnvIO = EnvT IO

runEnvT = runReaderT . unwrapEnv
runEnv = runReader . unwrapEnv

getEnv l = get l <$> ask

onFst = lens fst (\x (_, y) -> (x, y))
onSnd = lens snd (\y (x, _) -> (x, y))

envToConfig ic d col = do (Settings r c w) <- ask
                          return $ T.Config ic r d c w col

specToSettings spec = Settings r c w
    where rdr = get render $ spec
          c = uncurry (:+) . P.toTuple . getCd2 $ get windowCenter rdr
          w = P.spFst . getCd2 $ get windowSize rdr
          r = P.toTuple . getCd2 $ get outputSize rdr
