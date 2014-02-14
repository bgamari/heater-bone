import Control.Monad.Trans.Maybe
import qualified LCD
import LCD (LCD, LCDPins(..))
import qualified LED

lcdPins = LCDPins { rsPin  = 81
                  , ePin   = 87
                  , dbPins = [22, 27, 65, 46]
                  }

backlight = "heater:white:lcd"

main = do 
    lcd <- LCD.open lcdPins
    Just bklt <- runMaybeT $ LED.open backlight
    LED.setBrightness (LED.brightness 0.9) bklt
    LCD.home lcd
    LCD.write "Hello World!" lcd

