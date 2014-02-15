module GPIO ( PinNumber
            , Direction(..)
            , export
            , GPIO
            , open
            , close
            , write
            ) where

import System.IO
import Control.Applicative

type PinNumber = Int

data Direction = In | Out

newtype GPIO = GPIO Handle

export :: PinNumber -> IO ()
export gpioN = writeFile "/sys/class/gpio/export" (show gpioN)

open :: PinNumber -> Direction -> IO GPIO
open gpioN dir = do
    let d = "/sys/class/gpio/gpio"++show gpioN
    writeFile (d++"/direction") $ case dir of
      In  -> "in"
      Out -> "out"
    GPIO <$> openFile (d++"/value") mode
  where
    mode = case dir of
      In  -> ReadMode
      Out -> WriteMode

close :: GPIO -> IO ()
close (GPIO h) = hClose h

write :: Bool -> GPIO -> IO ()
write v (GPIO h) = hPutStr h v'
  where
    v' = case v of
      True  -> "1"
      False -> "0"


