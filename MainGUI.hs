module MainGUI where

import Control.Exception (bracket_)
import Control.Monad.IO.Class
import GHC.Word
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events (Event(..))

import Types hiding (Config(..))
import Settings
import Util
import Interval
import Roots
import IFS
import Plotting
import ParseConfig
import GUI.Pixels
import Rendering.Raster

type Time = Word32

-- guiMain :: (Integral v, Real a, Coefficient a) 
--         => Mode -> Config (Sum v) RGB8 a -> IO ()
-- guiMain :: Mode -> (a -> (InpCoord, v)) -> IterCoeffs a -> Gradient v RGB8 -> EnvIO ()
guiMain mode f ic g = do cfg <- envToConfig ic g
                         let roots = getRoots cfg
                         bracketSDL mode $ runMainLoop mode f roots g

bracketEnvIO :: EnvIO a -> EnvIO b -> EnvIO c -> EnvIO c
bracketEnvIO a z x = do env <- ask
                        liftIO $ bracket_ (runEnvT a env) (runEnvT z env) (runEnvT x env)

bracketSDL :: Mode -> EnvIO b -> EnvIO b
bracketSDL mode = bracketEnvIO (initSDL mode) endSDL

initSDL :: Mode -> EnvIO ()
initSDL mode = do liftIO $ SDL.init [SDL.InitEverything]
                  (w, h) <- getDisplaySize mode
                  liftIO $ SDL.setVideoMode w h 32 []
                  return ()

getDisplaySize :: Mode -> EnvIO (Int, Int)
getDisplaySize Both = modify onFst (*2) <$> getEnv resolution
getDisplaySize _    = getEnv resolution

endSDL :: EnvIO ()
endSDL = liftIO SDL.quit

runMainLoop :: (Monoid v) => Mode -> (a -> (InpCoord, v)) -> IterCoeffs a 
            -> Gradient v RGB8 -> EnvIO ()
runMainLoop mode f xs g = do rst <- createRasterizerIO f
                             mainLoop rst g xs

drawPixel :: RasterizerIO i v -> Gradient v RGB8 -> SDL.Surface -> i -> IO ()
drawPixel rst g surf p = do r <- rasterize rst p
                            renderPixel (fst g) surf `whenJust` r

renderPixel :: (v -> RGB8) -> SDL.Surface -> (XY, v) -> IO ()
renderPixel f surf (xy, v) = setPixel xy (f v) surf

mainLoop :: RasterizerIO i v -> Gradient v RGB8 -> [i] -> EnvIO ()
mainLoop rst g xs = do xs' <- withMinDelay 5 (timedDraw rst g 5 xs)
                       handleEvents rst g xs' =<< liftIO newEvents

handleEvents :: RasterizerIO i v -> Gradient v RGB8 -> [i] -> [Event] -> EnvIO ()
handleEvents rst g xs evs | done      = return ()
                          | otherwise = do mapM_ handleClicks clicks
                                           mainLoop rst g xs
  where done = any (== Quit) evs
        clicks = mapMaybe getClick evs

getClick (MouseButtonDown x y SDL.ButtonLeft) = Just (x, y)
getClick _ = Nothing

handleClicks :: (Word16, Word16) -> EnvIO ()
-- handleClicks (Config ic (rx,ry) d c w g) (x, y) = print (xy + c)
handleClicks (x, y) = do sz <- asks size
                         res <- getEnv resolution
                         c <- getEnv center
                         liftIO $ print (getC res sz + c)
  where [x', y'] = fromIntegral <$> [x, y]
--         [rx', ry'] = fromIntegral <$> [rx, ry]
--         h = w * ry'/rx' 
        getC (rx, ry) (w, h) = ((x' / fromIntegral rx - 0.5) * w) 
                            :+ ((y' / fromIntegral ry - 0.5) * h)
              

withMinDelay :: Time -> EnvIO a -> EnvIO a
withMinDelay dt x = do t1 <- liftIO SDL.getTicks
                       r <- x
                       liftIO $ delayUntil $ t1 + dt
                       return r

delayUntil t = do cur <- SDL.getTicks
                  when (cur < t) (SDL.delay $ t - cur)


timedDraw :: RasterizerIO i v -> Gradient v RGB8 -> Time -> [i] -> EnvIO [i]
timedDraw _ _ _ [] = return []
timedDraw rst g dt xs = do cur <- liftIO $ SDL.getTicks
                           s <- liftIO $ SDL.getVideoSurface
                           xs' <- liftIO $ withLock (untilTime (drawPixel rst g) (cur + dt) xs) s
                           liftIO $ SDL.flip s
                           when (null xs') (liftIO $ putStrLn "Plotting complete.")
                           return xs'


untilTime _ _ [] _ = return []
untilTime f t (x:xs) y = do f y x
                            cur <- SDL.getTicks
                            if cur < t then untilTime f t xs y else return xs

newEvents :: IO [Event]
newEvents = unfoldActionM (justIf (/= NoEvent) <$> SDL.pollEvent)

