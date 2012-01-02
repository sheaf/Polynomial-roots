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
runMainLoop mode cfg = do mainLoop (gradient cfg) (getPixels mode cfg)

getPixels :: (Real a, Coefficient a ) => Mode -> Config c a -> [Pixel]
getPixels Roots cfg = rootsPixels cfg
getPixels IFS cfg = ifsPixels cfg
getPixels Both cfg = interleave (rootsPixels cfg) (offset <$> ifsPixels cfg)
  where (rx, _) = resolution cfg
        interleave (x:xs) (y:ys) = x:y:interleave xs ys
        interleave [] ys = ys
        interleave xs [] = xs
        offset (x, y) = (x + rx, y)


drawPixel g surf xy = mapPixel xy (fst g) surf

mainLoop :: Gradient RGB8 -> [XY] -> IO ()
mainLoop g xs = do xs' <- withMinDelay 5 (timedDraw g 5 xs)
                   done <- any (== Quit) <$> newEvents
                   if done then return () else mainLoop g xs'

withMinDelay :: Time -> IO a -> IO a
withMinDelay dt x = do t1 <- SDL.getTicks
                       r <- x
                       delayUntil $ t1 + dt
                       return r

delayUntil t = do cur <- SDL.getTicks
                  when (cur < t) (SDL.delay $ t - cur)


timedDraw :: Gradient RGB8 -> Time -> [XY] -> IO [XY]
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

-- waitQuit = do evs <- newEvents
--               if any (== Quit) evs then return () else SDL.delay 80 >> waitQuit

