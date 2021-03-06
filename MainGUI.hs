{-# LANGUAGE TypeFamilies #-}
module MainGUI where

import Overture
import Prelude ()

import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader(ask, asks)
import Data.Colour
import Data.Foldable (Foldable(), toList)
import GHC.Word (Word16, Word32)
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Events (Event(..))
import System.Directory (doesFileExist)

import Image (dumpImage)
import Pair (Pair(toTuple))
import GUI.Pixels (withLock, setPixel)
import Rendering.Raster
import Rendering.Colour
import Rendering.Coord (Cd2, RstSize)
import qualified Settings as S (size)
import Settings hiding (size)
import Types hiding (Config(..))
import Util

type Time = Word32

guiMain :: (Foldable f, ColourScheme c, Rasterizer r, RstContext r ~ IO) 
        => f i -> r v i (ColourData c) -> c -> EnvIO ()
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

runMainLoop :: (Foldable f, ColourScheme c, Rasterizer r, RstContext r ~ IO)
            => f i -> r v i (ColourData c) -> c -> EnvIO ()
runMainLoop xs rst c = do s <- liftIO SDL.getVideoSurface
                          liftIO $ SDL.fillRect s Nothing (SDL.Pixel col)
                          mainLoop (toList xs) rst c
                            where bgc = bg c
                                  col = rgbaToWord32 bgc

drawPixel :: (Rasterizer r, RstContext r ~ IO, ColourScheme c) 
            => r v i (ColourData c) -> c -> SDL.Surface -> i -> IO ()
drawPixel rst g surf p = do r <- rasterize rst p
                            renderPixel g surf `whenJust` r

renderPixel :: (ColourScheme c) => c -> SDL.Surface 
            -> (Cd2 Int, ColourData c) -> IO ()
renderPixel c surf (xy, v) = do let bgc = flip over black $ bg c
                                let col = flip over bgc $ (toColour c) v
                                setPixel (toTuple xy) (toRGB8 col) surf

mainLoop :: (Rasterizer r, ColourScheme c, RstContext r ~ IO) 
         => [i] -> r v i (ColourData c) -> c -> EnvIO ()
mainLoop xs rst g = do xs' <- withMinDelay 5 (timedDraw g rst 5 xs)
                       handleEvents xs' rst g =<< liftIO newEvents

handleEvents :: (Rasterizer r, ColourScheme c, RstContext r ~ IO) 
             => [i] -> r v i (ColourData c) -> c -> [Event] -> EnvIO ()
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

dumpGUIImage rst g = do fn <- nextImageName f
                        putStrLn $ "Saving to image: " ++ fn
                        dumpImage rst g fn
                        putStrLn "Image saved."
    where f n = "gui_dump_" ++ show n ++ ".png"

withMinDelay :: Time -> EnvIO a -> EnvIO a
withMinDelay dt x = do t1 <- liftIO SDL.getTicks
                       r <- x
                       liftIO $ delayUntil $ t1 + dt
                       return r

delayUntil t = do cur <- SDL.getTicks
                  when (cur < t) (SDL.delay $ t - cur)

timedDraw :: (Rasterizer r, RstContext r ~ IO, ColourScheme c) 
          => c -> r v i (ColourData c) -> Time -> [i] -> EnvIO [i]
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
