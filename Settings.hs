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
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative
import Control.Category (Category(), (>>>), (<<<))
import Data.Complex
import Data.Label (mkLabels)
import Data.Label.Pure

import Types hiding (Config(..))
import qualified Types as T

type Height = Double

data Settings = Settings { _resolution :: Resolution
                         , _degree     :: Degree
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

envToConfig ic = do (Settings r d c w) <- ask
                    return $ T.Config ic r d c w

cfgToSettings (T.Config _ r d c w _) = Settings r d c w

