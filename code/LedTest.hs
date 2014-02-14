import Prelude hiding (mapM_, sequence_)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Concurrent (threadDelay, forkIO)
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import LED

rgbNames :: RGB String
rgbNames = RGB "heater:status:red" "heater:status:green" "heater:status:blue"

encoderNames :: RGB String
encoderNames = RGB "heater:encoder:red" "heater:encoder:green" "heater:encoder:blue"

ledNames :: [String]
ledNames = map (++":green") roots ++ map (++":red") roots
  where
    roots = map (\n->"heater:led"++show n) [1..4]

delay = 1000*1000 `div` 30

instance Foldable RGB where
    foldMap f (RGB r g b) = f r <> f g <> f b

instance Traversable RGB where
    traverse f (RGB r g b) = RGB <$> f r <*> f g <*> f b

main = do
    Just rgb <- runMaybeT $ traverse LED.open rgbNames
    Just enc <- runMaybeT $ traverse LED.open encoderNames
    Just leds <- runMaybeT $ traverse LED.open ledNames
    traverse (setBrightness (brightness 0)) rgb
    mapM_ (\led->LED.setBrightness (brightness 1) led >> threadDelay (1000*1000) >> LED.setBrightness (brightness 0) led) leds
    forkIO $ mapM_ (\c->setRGB enc (fmap (scale 0.1) c) >> threadDelay delay) psychedelic
    mapM_ (\c->setRGB rgb (fmap (scale 0.1) c) >> threadDelay delay) psychedelic

setRGB :: RGB LED -> RGB Brightness -> IO ()
setRGB leds c = sequence_ $ LED.setBrightness <$> c <*> leds

psychedelic :: [RGB Brightness]
psychedelic = map (fmap brightness . go) [0,1e-3..]
  where
    go :: Double -> RGB Double
    go t = hsv h s v
      where 
        h = 360 * (0.5*sin t + 1)
        s = 0.5 + 0.1*sin (t / sqrt 3)
        v = 0.5
