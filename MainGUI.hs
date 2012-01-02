module MainGUI where

import Control.Exception (bracket_)
import GHC.Word
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events (Event(..))

import Types
import Util
import Interval
import Roots
import IFS
import Plotting
import Image
import ParseConfig
import GUI.Pixels

type Time = Word32

guiMain :: (Real a, Coefficient a) => Mode -> Config RGB8 a -> IO ()
guiMain mode cfg = bracketSDL mode cfg $ runMainLoop mode cfg

bracketSDL :: Mode -> Config c a -> IO b -> IO b
bracketSDL mode cfg = bracket_ (initSDL mode cfg) endSDL 

initSDL :: Mode -> Config c a -> IO ()
initSDL mode cfg = do SDL.init [SDL.InitEverything]
                      SDL.setVideoMode w h 32 []
                      return ()
  where (w, h) = getDisplaySize mode cfg

getDisplaySize :: Mode -> Config c a -> (Int, Int)
getDisplaySize Both (Config _ (rx,ry) _ _ _ _) = (rx*2, ry)
getDisplaySize _    (Config _ (rx,ry) _ _ _ _) = (rx, ry)

endSDL :: IO ()
endSDL = SDL.quit

runMainLoop :: (Real a, Coefficient a ) => Mode -> Config RGB8 a -> IO ()
runMainLoop mode cfg = do mainLoop cfg (getPixels mode cfg)

getPixels :: (Real a, Coefficient a ) => Mode -> Config c a -> [PlotData]
getPixels Roots cfg = plotPixels cfg (getRoots cfg)
getPixels IFS cfg = plotPixels cfg (ifsPoints cfg)
getPixels Both cfg = interleave rootsPxs (offset <$> ifsPxs)
  where (rx, _) = resolution cfg
        rootsPxs = plotPixels cfg (getRoots cfg)
        ifsPxs = plotPixels cfg (ifsPoints cfg)
        interleave (x:xs) (y:ys) = x:y:interleave xs ys
        interleave [] ys = ys
        interleave xs [] = xs
        offset ((x, y), o) = ((x + rx, y), o)

drawPixel :: Gradient RGB8 -> SDL.Surface -> (Pixel, PixelOrig) -> IO ()
drawPixel g surf (xy,o) = mapPixel xy (fst g) surf

mainLoop :: Config RGB8 a -> [PlotData] -> IO ()
mainLoop cfg xs = do xs' <- withMinDelay 5 (timedDraw (gradient cfg) 5 xs)
                     handleEvents cfg xs' =<< newEvents

handleEvents :: Config RGB8 a -> [PlotData] -> [Event] -> IO ()
handleEvents cfg xs evs | done      = return ()
                        | otherwise = do mapM_ (handleClicks cfg) clicks
                                         mainLoop cfg xs
  where done = any (== Quit) evs
        clicks = mapMaybe getClick evs

getClick (MouseButtonDown x y SDL.ButtonLeft) = Just (x, y)
getClick _ = Nothing

handleClicks :: Config RGB8 a -> (Word16, Word16) -> IO ()
handleClicks (Config ic (rx,ry) d c w g) (x, y) = print (xy + c)
  where [x', y'] = fromIntegral <$> [x, y]
        [rx', ry'] = fromIntegral <$> [rx, ry]
        h = w * ry'/rx' 
        xy = ((x' / rx' - 0.5) * w) :+ ((y' / ry' - 0.5) * h)
        
        

withMinDelay :: Time -> IO a -> IO a
withMinDelay dt x = do t1 <- SDL.getTicks
                       r <- x
                       delayUntil $ t1 + dt
                       return r

delayUntil t = do cur <- SDL.getTicks
                  when (cur < t) (SDL.delay $ t - cur)


timedDraw :: Gradient RGB8 -> Time -> [PlotData] -> IO [PlotData]
timedDraw _ _ [] = return []
timedDraw g dt xs = do cur <- SDL.getTicks
                       s <- SDL.getVideoSurface
                       xs' <- withLock (untilTime (drawPixel g) (cur + dt) xs) s
                       SDL.flip s
                       when (null xs') (putStrLn "Plotting complete.")
                       return xs'
                    

untilTime _ _ [] _ = return []
untilTime f t (x:xs) y = do f y x
                            cur <- SDL.getTicks
                            if cur < t then untilTime f t xs y else return xs

newEvents :: IO [Event]
newEvents = unfoldActionM (justIf (/= NoEvent) <$> SDL.pollEvent)

