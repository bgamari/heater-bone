import Control.Error
import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified LCD
import LCD (LCD, LCDPins(..))
import qualified LED

lcdPins = LCDPins { rsPin  = 81
                  , ePin   = 87
                  , dbPins = LCD.Nibble 22 27 65 46
                  }

backlight = "heater:white:lcd"

readTemp :: EitherT String IO Double
readTemp = do
    v <- liftIO $ readFile "/sys/class/hwmon/hwmon0/device/temp2_input"
    (/1000) <$> tryRead "readTemp: Parse error" v

main = do 
    lcd <- LCD.open lcdPins
    Just bklt <- runMaybeT $ LED.open backlight
    writeFile "/sys/class/hwmon/hwmon0/device/channel_sel" "ext\n"
    LED.setBrightness (LED.brightness 0.9) bklt
    LCD.queue lcd $ do
      LCD.home
      LCD.write "Hello World!"
      LCD.setCursor False
      LCD.setBlink False
      LCD.setDisplay True
    forever $ runEitherT $ do
      a <- readTemp
      liftIO $ setLCD lcd $ show a++"deg C"
      liftIO $ threadDelay (1000*1000)

setLCD :: LCD -> String -> IO ()
setLCD lcd s = LCD.queue lcd $ do
    LCD.home
    LCD.clear
    LCD.write s

