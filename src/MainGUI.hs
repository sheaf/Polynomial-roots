
{-# LANGUAGE OverloadedStrings #-}

module MainGUI where

-- base
import Control.Exception
  ( bracket )
import Control.Monad
  ( when )
import Data.Complex
import Data.Foldable
  ( for_, toList )
import Data.Maybe
  ( mapMaybe )
import GHC.Exts
  ( RealWorld )
import GHC.Word
  ( Word16, Word32 )

-- colour
import Data.Colour

-- JuicyPixels
import qualified Codec.Picture as JuicyPixels
import qualified Codec.Picture.Types as JuicyPixels

-- mtl
import Control.Monad.Reader
  ( ask, asks )

-- sdl2
import qualified SDL
import SDL.Event
--import qualified Graphics.UI.SDL as SDL
--import Graphics.UI.SDL.Keysym
--import Graphics.UI.SDL.Events (Event(..))

-- spool
import qualified Data.Vector.Storable.ByteString as Spool
  ( vectorToByteString )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )

-- polynomial-roots
import Image
  ( dumpImage )
import Pair
  ( Pair(toTuple) )
import Rendering.Raster
import Rendering.Colour
import Rendering.Coord
  ( Cd2, RstSize )
import qualified Settings as S
  ( size )
import Settings
  hiding ( size )
import Types
  hiding ( Config(..) )
import Util

--------------------------------------------------------------------------------

type Time = Word32

guiMain :: (Foldable f, ColourScheme c, Rasterizer r, RstContext r ~ IO)
        => f i -> r v i (ColourData c) -> c -> EnvIO ()
guiMain xs rst g = bracketSDL (outputSize rst) $ runMainLoop xs rst g

bracketEnvIO :: EnvIO a -> ( a -> EnvIO b ) -> ( a -> EnvIO c ) -> EnvIO c
bracketEnvIO a z x =
  do env <- ask
     liftIO $ bracket (runEnvT a env) (\ b -> runEnvT ( z b ) env)
                (\ b -> runEnvT ( x b ) env)

bracketSDL :: RstSize -> ( SDL_Context  -> EnvIO b ) -> EnvIO b
bracketSDL sz = bracketEnvIO (initSDL sz) endSDL


type Image = JuicyPixels.MutableImage RealWorld JuicyPixels.PixelRGBA8

data SDL_Context =
  SDL_Context
    { renderer :: !SDL.Renderer
    , window   :: !SDL.Window
    , texture  :: !SDL.Texture
    , image    :: !Image
    , pitch    :: !Int }

initSDL :: RstSize -> EnvIO SDL_Context
initSDL sz = do
  SDL.initializeAll
  let (w, h) = getDisplaySize sz
      wh = SDL.V2 (fromIntegral w) (fromIntegral h)
  window <-
    SDL.createWindow "Polynomial roots" $
      SDL.defaultWindow
        { SDL.windowInitialSize = wh
        , SDL.windowBorder = False }
  renderer <-
    SDL.createRenderer window -1 $
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = True --False
        }
  texture <-
    SDL.createTexture renderer SDL.ABGR8888 SDL.TextureAccessStreaming wh
  image <- liftIO $ JuicyPixels.newMutableImage @JuicyPixels.PixelRGBA8 w h
  let pitch = w * 4
  return ( SDL_Context { .. } )


getDisplaySize :: RstSize -> (Int, Int)
getDisplaySize = toTuple

endSDL :: SDL_Context -> EnvIO ()
endSDL ctxt =
  do immage <- liftIO $ JuicyPixels.unsafeFreezeImage $ image ctxt
     liftIO $ JuicyPixels.savePngImage "C:/Docs/roots_test.png" (JuicyPixels.ImageRGBA8 immage)
     SDL.destroyWindow ( window ctxt )
     SDL.quit

runMainLoop :: (Foldable f, ColourScheme c, Rasterizer r, RstContext r ~ IO)
            => f i -> r v i (ColourData c) -> c -> SDL_Context  -> EnvIO ()
runMainLoop xs rst c ctxt@( SDL_Context { renderer, image } ) =
  do liftIO $ putStrLn "Starting main loop"
     SDL.clear renderer
     liftIO $ JuicyPixels.fillImageWith image col
     presentImageData ctxt
     mainLoop (toList xs) rst c ctxt
       where col = toRGBA JuicyPixels.PixelRGBA8 ( bg c )

drawPixel :: (Rasterizer r, RstContext r ~ IO, ColourScheme c)
            => r v i (ColourData c) -> c -> Image -> i -> IO ()
drawPixel rst g img p =
  do mb_r <- rasterize rst p
     for_ mb_r $ renderPixel g img

renderPixel :: (ColourScheme c) => c -> Image
            -> (Cd2 Int, ColourData c) -> IO ()
renderPixel c img (xy, v) = do
  let bgc = flip over black $ bg c
  let col = flip over bgc $ (toColour c) v
      px  = toRGBA JuicyPixels.PixelRGBA8 $ opaque col
      (x,y) = toTuple xy
  JuicyPixels.writePixel img x y px

mainLoop :: (Rasterizer r, ColourScheme c, RstContext r ~ IO)
         => [i] -> r v i (ColourData c) -> c -> SDL_Context -> EnvIO ()
mainLoop xs rst g ctxt =
  do xs' <- withMinDelay 5 (timedDraw g rst ctxt 5 xs)
     handleEvents xs' rst g ctxt =<< liftIO newEvents

handleEvents :: (Rasterizer r, ColourScheme c, RstContext r ~ IO)
             => [i] -> r v i (ColourData c) -> c -> SDL_Context -> [SDL.Event] -> EnvIO ()
handleEvents xs rst g ctxt evs
  | done      = return ()
  | otherwise = do mapM_ handleClicks clicks
                   mapM_ (handleKeys rst g) keys
                   mainLoop xs rst g ctxt
  where done = any (isQuit . eventPayload) evs
        clicks = mapMaybe (getClick . eventPayload) evs
        keys = mapMaybe (getKeys . eventPayload) evs

isQuit :: EventPayload -> Bool
isQuit SDL.QuitEvent = True
isQuit evt
  | Just (SDL.Keysym SDL.ScancodeEscape _ _) <- getKeys evt
  = True
  | otherwise
  = False

getClick :: EventPayload -> Maybe (Word16, Word16)
getClick (MouseButtonEvent evt)
  | SDL.ButtonLeft <- mouseButtonEventButton evt
  , SDL.P ( SDL.V2 x y ) <- mouseButtonEventPos evt
  = Just ( fromIntegral x, fromIntegral y)
getClick _ = Nothing

handleClicks :: (Word16, Word16) -> EnvIO ()
handleClicks (x, y) = do sz <- asks S.size
                         res <- resolution <$> ask
                         c <- center <$> ask
                         liftIO $ print (getC res sz + c)
  where x' = fromIntegral x
        y' = fromIntegral y
        getC (rx, ry) (w, h) = ((x' / fromIntegral rx - 0.5) * w)
                            :+ ((y' / fromIntegral ry - 0.5) * h)

getKeys :: EventPayload -> Maybe SDL.Keysym
getKeys (KeyboardEvent evt)
  | SDL.Pressed <- SDL.keyboardEventKeyMotion evt
  , let ks = SDL.keyboardEventKeysym evt
  = Just ks
getKeys _ = Nothing

handleKeys :: (RstContext r ~ IO, MonadIO m, Rasterizer r, ColourScheme c)
           => r v i (ColourData c) -> c -> SDL.Keysym -> m ()
handleKeys rst g (SDL.Keysym SDL.ScancodeS _ _) = liftIO $ dumpGUIImage rst g
handleKeys _ _ _ = return ()

dumpGUIImage :: (RstContext r ~ IO, Rasterizer r, ColourScheme c)
             => r v i (ColourData c) -> c -> IO ()
dumpGUIImage rst g =
  do fn <- nextImageName f
     putStrLn $ "Saving to image: " ++ fn
     dumpImage rst g fn
     putStrLn "Image saved."
    where
      f :: Show a => a -> [Char]
      f n = "gui_dump_" ++ show n ++ ".png"

withMinDelay :: Time -> EnvIO a -> EnvIO a
withMinDelay dt x =
  do t1 <- SDL.ticks
     r <- x
     delayUntil $ t1 + dt
     return r

delayUntil :: MonadIO m => Word32 -> m ()
delayUntil t =
  do cur <- SDL.ticks
     when (cur < t) (SDL.delay $ t - cur)

timedDraw :: (Rasterizer r, RstContext r ~ IO, ColourScheme c)
          => c -> r v i (ColourData c) -> SDL_Context -> Time -> [i] -> EnvIO [i]
timedDraw _ _ _ _ [] = return []
timedDraw g rst ctxt@( SDL_Context { image } ) dt xs =
  do cur <- SDL.ticks
     xs' <- liftIO $ untilTime (drawPixel rst g) (cur + dt) xs image
     presentImageData ctxt
     when (null xs') (liftIO $ putStrLn "Plotting complete.")
     return xs'

presentImageData :: SDL_Context -> EnvIO ()
presentImageData ( SDL_Context { renderer, image, texture, pitch } ) = do
  image_bs <-
    liftIO $ Spool.vectorToByteString . JuicyPixels.imageData <$> JuicyPixels.freezeImage image
  SDL.updateTexture texture Nothing image_bs ( fromIntegral pitch )
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer

untilTime :: (a -> b -> IO ()) -> Time -> [b] -> a -> IO [b]
untilTime _ _ [] _ = return []
untilTime f t (x:xs) y =
  do f y x
     cur <- SDL.ticks
     if cur < t then untilTime f t xs y else return xs

newEvents :: IO [Event]
newEvents = unfoldActionM SDL.pollEvent
