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
import ParseConfig
import GUI.Pixels
import Rendering.Raster

type Time = Word32

guiMain :: (Integral v, Real a, Coefficient a) 
        => Mode -> Config (Sum v) RGB8 a -> IO ()
guiMain mode cfg = bracketSDL mode cfg $ runMainLoop mode cfg

bracketSDL :: Mode -> Config v c a -> IO b -> IO b
bracketSDL mode cfg = bracket_ (initSDL mode cfg) endSDL 

initSDL :: Mode -> Config v c a -> IO ()
initSDL mode cfg = do SDL.init [SDL.InitEverything]
                      SDL.setVideoMode w h 32 []
                      return ()
  where (w, h) = getDisplaySize mode cfg

getDisplaySize :: Mode -> Config v c a -> (Int, Int)
getDisplaySize Both (Config _ (rx,ry) _ _ _ _) = (rx*2, ry)
getDisplaySize _    (Config _ (rx,ry) _ _ _ _) = (rx, ry)

endSDL :: IO ()
endSDL = SDL.quit

runMainLoop :: (Integral v, Real a, Coefficient a ) 
            => Mode -> Config (Sum v) RGB8 a -> IO ()
runMainLoop mode cfg = do rst <- createRasterizerIO rootFunc cfg
                          mainLoop cfg rst roots
  where roots = getRoots cfg
        rootFunc (RootPlot _ r) = (complexToPair r, Sum 25)
        

drawPixel :: RasterizerIO (RootPlot a) v -> (Gradient v RGB8) 
          -> SDL.Surface -> RootPlot a -> IO ()
drawPixel rst g surf p = do r <- rasterize rst p
                            renderPixel (fst g) surf `whenJust` r

renderPixel :: (v -> RGB8) -> SDL.Surface -> (XY, v) -> IO ()
renderPixel f surf (xy, v) = setPixel xy (f v) surf

mainLoop :: Config v RGB8 a -> RasterizerIO (RootPlot a) v -> [RootPlot a] -> IO ()
mainLoop cfg rst xs = do xs' <- withMinDelay 5 (timedDraw rst (gradient cfg) 5 xs)
                         handleEvents cfg rst xs' =<< newEvents

handleEvents :: Config v RGB8 a -> RasterizerIO (RootPlot a) v 
             -> [RootPlot a] -> [Event] -> IO ()
handleEvents cfg rst xs evs | done      = return ()
                            | otherwise = do mapM_ (handleClicks cfg) clicks
                                             mainLoop cfg rst xs
  where done = any (== Quit) evs
        clicks = mapMaybe getClick evs

getClick (MouseButtonDown x y SDL.ButtonLeft) = Just (x, y)
getClick _ = Nothing

handleClicks :: Config v RGB8 a -> (Word16, Word16) -> IO ()
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


timedDraw :: RasterizerIO (RootPlot a) v -> (Gradient v RGB8) 
          -> Time -> [RootPlot a] -> IO [RootPlot a]
timedDraw _ _ _ [] = return []
timedDraw rst g dt xs = do cur <- SDL.getTicks
                           s <- SDL.getVideoSurface
                           xs' <- withLock (untilTime (drawPixel rst g) (cur + dt) xs) s
                           SDL.flip s
                           when (null xs') (putStrLn "Plotting complete.")
                           return xs'
                    

untilTime _ _ [] _ = return []
untilTime f t (x:xs) y = do f y x
                            cur <- SDL.getTicks
                            if cur < t then untilTime f t xs y else return xs

newEvents :: IO [Event]
newEvents = unfoldActionM (justIf (/= NoEvent) <$> SDL.pollEvent)

