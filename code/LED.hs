module LED ( -- * LED brightness
	     Brightness
	   , brightness
	   , scale
	     -- * LED
	   , LED
	   , open
	   , close
	   , setBrightness
	   ) where

import System.IO
import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

data LED = LED Int Handle
	 deriving (Show, Eq)

newtype Brightness = Brightness Double
		   deriving (Ord, Eq)

brightness :: Double -> Brightness
brightness b
  | b > 1     = Brightness 1
  | b < 0     = Brightness 0
  | otherwise = Brightness b

scale :: Double -> Brightness -> Brightness
scale a (Brightness b) = brightness $ b * a

open :: String -> MaybeT IO LED
open name = do
    max <- liftIO $ read <$> readFile (root<>"max_brightness")
    h <- liftIO $ openFile (root<>"brightness") WriteMode
    return (LED max h)
  where root = "/sys/class/leds/"<>name<>"/"

close :: LED -> IO ()
close (LED _ h) = hClose h

setBrightness :: Brightness -> LED -> IO ()
setBrightness (Brightness b) (LED max h) = do
    hPutStr h (s <> "\0")
    hFlush h
  where s = show (round $ b * realToFrac max)

