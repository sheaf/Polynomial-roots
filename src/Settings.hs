module Settings where

-- base
import Control.Monad.Identity
  ( Identity )
import Data.Complex

-- mtl
import Control.Monad.Reader

-- polynomial-roots
import Configuration
  ( RenderSpec(..), RunSpec(..) )
import qualified Configuration as Window
  ( WindowSpec(..) )
import qualified Pair as P
  ( StrictPair(spFst), Pair(toTuple) )
import Types
  ( Width, Center, Resolution )
import Rendering.Coord
  ( Cd2(getCd2) )

--------------------------------------------------------------------------------

type Height = Double

data Settings
  = Settings
  { resolution :: Resolution
  , center     :: Center
  , width      :: Width
  }

aspect :: Settings -> Double
aspect st = let (rx,ry) = resolution st
            in fromIntegral rx / fromIntegral ry

height :: Settings -> Height
height s = width s / aspect s
  -- (\h s -> set width (h * aspect s) s)

size :: Settings -> (Width, Height)
size s = (width s, height s)

newtype EnvT m a = Env { unwrapEnv :: ReaderT Settings m a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Settings, MonadIO
    )

type Env = EnvT Identity
type EnvIO = EnvT IO

runEnvT :: EnvT m a -> Settings -> m a
runEnvT = runReaderT . unwrapEnv
runEnv :: EnvT Identity a -> Settings -> a
runEnv = runReader . unwrapEnv

--envToConfig ic d col = do (Settings r c w) <- ask
--                          return $ T.Config ic r d c w col

specToSettings :: RunSpec -> Settings
specToSettings spec = Settings r c w
    where rdr = render $ spec
          c = uncurry (:+) . P.toTuple . getCd2 $ Window.center $ windowSpec rdr
          w = P.spFst . getCd2 $ Window.size $ windowSpec rdr
          r = P.toTuple . getCd2 $ outputSize rdr
