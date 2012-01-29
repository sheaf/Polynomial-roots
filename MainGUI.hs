{-# LANGUAGE TypeFamilies #-}
module MainGUI where

import Control.Exception (bracket_)
import Control.Monad.IO.Class
import GHC.Word
import System.Directory
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Events (Event(..))
import Data.Foldable (Foldable(), toList)
import Data.Colour

import Types hiding (Config(..), Gradient, outputSize)
import Settings hiding (size)
import qualified Settings as S
import Util
import Image
import Pair
import GUI.Pixels
import Rendering.Raster
import Rendering.ArrayRaster
import Rendering.Gradient
import Rendering.Colour
import Rendering.Coord
import Types(Gradient(..))

type Time = Word32

guiMain :: (Foldable f, Monoid m, Rasterizer r, RstContext r ~ IO) 
        => f i -> r v i m
        -> Gradient m Colour Double -> EnvIO ()
guiMain xs rst g = bracketSDL (outputSize rst) $ runMainLoop xs rst g

bracketEnvIO :: EnvIO a -> EnvIO b -> EnvIO c -> EnvIO c
bracketEnvIO a z x = do env <- ask
                        liftIO $ bracket_ (runEnvT a env) (runEnvT z env) (runEnvT x env)

bracketSDL :: RstSize -> EnvIO b -> EnvIO b
bracketSDL sz = bracketEnvIO (initSDL sz) endSDL

initSDL :: RstSize -> EnvIO ()
initSDL sz = do liftIO $ SDL.init [SDL.InitEverything]
                let (w, h) = getDisplaySize sz
                liftIO $ SDL.setVideoMode w h 32 []
                return ()

getDisplaySize :: RstSize -> (Int, Int)
getDisplaySize = toTuple

endSDL :: EnvIO ()
endSDL = liftIO SDL.quit

runMainLoop :: (Foldable f, Monoid m, Rasterizer r, RstContext r ~ IO) 
            => f i -> r v i m
            -> Gradient m Colour Double -> EnvIO ()
runMainLoop xs rst g = mainLoop (toList xs) rst g

drawPixel :: (Rasterizer r, RstContext r ~ IO) 
            => r v i m -> Gradient m Colour Double 
            -> SDL.Surface -> i -> IO ()
drawPixel rst g surf p = do r <- rasterize rst p
                            renderPixel g surf `whenJust` r

renderPixel :: Gradient m Colour Double -> SDL.Surface 
            -> (Cd2 Int, m) -> IO ()
renderPixel g surf (xy, v) = do let c = runGrad g v
                                setPixel (toTuple xy) (toRGB8 c) surf

mainLoop :: (Rasterizer r, Monoid m, RstContext r ~ IO) 
         => [i] -> r v i m
         -> Gradient m Colour Double -> EnvIO ()
mainLoop xs rst g = do xs' <- withMinDelay 5 (timedDraw g rst 5 xs)
                       handleEvents xs' rst g =<< liftIO newEvents

handleEvents :: (Rasterizer r, Monoid m, RstContext r ~ IO) 
             => [i] -> r v i m -> Gradient m Colour Double 
             -> [Event] -> EnvIO ()
handleEvents xs rst g evs | done      = return ()
                          | otherwise = do mapM_ handleClicks clicks
                                           mapM_ (handleKeys rst g) keys
                                           mainLoop xs rst g
  where done = any (== Quit) evs
        clicks = mapMaybe getClick evs
        keys = mapMaybe getKeys evs

getClick (MouseButtonDown x y SDL.ButtonLeft) = Just (x, y)
getClick _ = Nothing

handleClicks :: (Word16, Word16) -> EnvIO ()
handleClicks (x, y) = do sz <- asks S.size
                         res <- getEnv resolution
                         c <- getEnv center
                         liftIO $ print (getC res sz + c)
  where [x', y'] = fromIntegral <$> [x, y]
        getC (rx, ry) (w, h) = ((x' / fromIntegral rx - 0.5) * w) 
                            :+ ((y' / fromIntegral ry - 0.5) * h)

getKeys (KeyDown ks) = Just ks
getKeys _ = Nothing

handleKeys rst g (Keysym SDLK_s _ _) = liftIO $ dumpGUIImage rst g
handleKeys _ _ _ = return ()

dumpGUIImage rst g = do fn <- nextImageName $ mkImageName <$> [1..]
                        putStrLn $ "Saving to image: " ++ fn
                        dumpImage rst g fn
                        putStrLn $ "Image saved."
  where mkImageName n = "gui_dump_" ++ show n ++ ".png"
        nextImageName (n:ns) = do exists <- doesFileExist n
                                  if exists then nextImageName ns else return n

withMinDelay :: Time -> EnvIO a -> EnvIO a
withMinDelay dt x = do t1 <- liftIO SDL.getTicks
                       r <- x
                       liftIO $ delayUntil $ t1 + dt
                       return r

delayUntil t = do cur <- SDL.getTicks
                  when (cur < t) (SDL.delay $ t - cur)


timedDraw :: (Rasterizer r, RstContext r ~ IO) 
          => Gradient m Colour Double -> r v i m
          -> Time -> [i] -> EnvIO [i]
timedDraw _ _ _ [] = return []
timedDraw g rst dt xs = do cur <- liftIO SDL.getTicks
                           s <- liftIO SDL.getVideoSurface
                           xs' <- liftIO $ withLock (untilTime (drawPixel rst g) (cur + dt) xs) s
                           liftIO $ SDL.flip s
                           when (null xs') (liftIO $ putStrLn "Plotting complete.")
                           return xs'

untilTime :: (a -> b -> IO ()) -> Time -> [b] -> a -> IO [b]
untilTime _ _ [] _ = return []
untilTime f t (x:xs) y = do f y x
                            cur <- SDL.getTicks
                            if cur < t then untilTime f t xs y else return xs

newEvents :: IO [Event]
newEvents = unfoldActionM (justIf (/= NoEvent) <$> SDL.pollEvent)

