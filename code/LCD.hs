{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module LCD ( LCDPins(..)
	   , LCD
           , open
           , clear
           , home
           , write
           ) where

import Data.Word
import Data.Bits
import Data.Char (ord)
import Control.Concurrent (threadDelay)
import qualified GPIO as GPIO
import GPIO (GPIO, PinNumber)
import System.IO
import Control.Applicative
import Data.Traversable as T
import Data.Foldable

data LCDPins a = LCDPins { rsPin, ePin :: a
	                 , dbPins :: [a] -- | DB4 - DB7
                         }
   	       deriving (Functor, Foldable, Traversable)

data LCD = LCD { lcdPins :: LCDPins GPIO }

open :: LCDPins PinNumber -> IO LCD
open pins = do
    --traverse GPIO.export pins
    lcd <- LCD <$> traverse (\n->GPIO.open n GPIO.Out) pins

    command Reg 0x33 lcd
    command Reg 0x32 lcd
    command Reg 0x27 lcd -- 2-line 5x7 matrix
    command Reg 0x0c lcd -- turn off cursor
    command Reg 0x06 lcd -- shift cursor right
    clear lcd
    return lcd

data RS = Reg | RAM

bits :: Bits a => a -> [Bool]
bits a = go 0
  where
    go i | i < bitSize a  = testBit a i : go (i+1)
         | otherwise      = []

command :: RS -> Word8 -> LCD -> IO ()
command rs d (LCD pins) = do
    threadDelay (1000*1000)
    flip GPIO.write (rsPin pins) $ case rs of
      Reg -> False
      RAM -> True
    nibble (d `shiftR` 4)
    nibble d
  where
    nibble :: Word8 -> IO ()
    nibble n = do
      T.sequence $ GPIO.write <$> take 4 (bits n) <*> dbPins pins
      pulseEnable

    pulseEnable = do
      forM_ [False, True, False] $ \v->do
        GPIO.write v (ePin pins)
        threadDelay 1

clear :: LCD -> IO ()
clear = command Reg 0x01

home :: LCD -> IO ()
home = command Reg 0x02

write :: String -> LCD -> IO ()
write s lcd = go s
  where
    go []          = return ()
    go ('\n':rest) = command Reg 0xc0 lcd >> go rest
    go (c:rest)    = command RAM (fromIntegral $ ord c) lcd >> go rest

