{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module LCD ( LCDPins(..)
           , LCD
           , open
           , queue
             -- * Actions
           , clear
           , home
           , write
           , setDisplay
           , setCursor
           , setBlink
           ) where

import Data.Word
import Data.Bits
import Data.Char (ord)
import qualified GPIO as GPIO
import GPIO (GPIO, PinNumber)
import System.IO
import Control.Monad (void, forever)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Traversable as T
import Data.Foldable
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO)

data LCDPins a = LCDPins { rsPin, ePin :: a
                         , dbPins :: [a] -- | DB4 - DB7
                         }
               deriving (Functor, Foldable, Traversable)

data RS = Reg | RAM

data LCDState = LCDSt { lcdDisplay :: Bool
                      , lcdCursor  :: Bool
                      , lcdBlink   :: Bool
                      }

type ActionM = StateT LCDState (ReaderT (LCDPins GPIO) IO)
type Action = ActionM ()

data LCD = LCD (TQueue Action)

open :: LCDPins PinNumber -> IO LCD
open pins = do
    --traverse GPIO.export pins
    lcdPins <- traverse (\n->GPIO.open n GPIO.Out) pins
    q <- newTQueueIO
    let lcd = LCD q
        s = LCDSt False False False

    queue lcd $ do
      command Reg 0x33
      command Reg 0x32
      command Reg 0x27 -- 2-line 5x7 matrix
      updateDisplayMode
      command Reg 0x06 -- shift cursor right
      clear
    
    forkIO $ void $ runReaderT (runStateT (worker q) s) lcdPins
    return lcd

worker :: TQueue Action -> Action
worker q = forever $ do
    action <- lift $ lift $ atomically $ readTQueue q
    action

bits :: Bits a => a -> [Bool]
bits a = go 0
  where
    go i | i < bitSize a  = testBit a i : go (i+1)
         | otherwise      = []

command :: RS -> Word8 -> Action
command rs d = lift $ ReaderT $ go
  where
    go :: LCDPins GPIO -> IO ()
    go pins = do
      threadDelay 1000
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

queue :: LCD -> Action -> IO ()
queue (LCD q) action = atomically $ writeTQueue q action

updateDisplayMode :: Action
updateDisplayMode = do
    s <- get
    let _bit :: Bits a => Int -> Bool -> a -> a
        _bit bit True  x = setBit x bit
        _bit bit False x = clearBit x bit
        v = _bit 2 (lcdDisplay s)
          $ _bit 1 (lcdCursor s)
          $ _bit 0 (lcdBlink s)
          $ 0x8
    command Reg v

modifyDisplayMode :: (LCDState -> LCDState) -> Action
modifyDisplayMode f = modify f >> updateDisplayMode

setDisplay :: Bool -> Action
setDisplay v = modifyDisplayMode $ \s->s {lcdDisplay = v}

setCursor :: Bool -> Action
setCursor v = modifyDisplayMode $ \s->s {lcdCursor = v}

setBlink :: Bool -> Action
setBlink v = modifyDisplayMode $ \s->s {lcdBlink = v}

clear :: Action
clear = command Reg 0x01

home :: Action
home = command Reg 0x02

write :: String -> Action
write s = go s
  where
    go []          = return ()
    go ('\n':rest) = command Reg 0xc0 >> go rest
    go (c:rest)    = command RAM (fromIntegral $ ord c) >> go rest

