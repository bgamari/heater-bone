import GPIO
import LCD (Nibble(..))
import Data.Traversable as T
import Control.Applicative

pins = Nibble 22 27 65 46

main = do
    --open 22 Out >>= write True
    pins' <- traverse (\n->GPIO.open n Out) pins
    --write True (head pins')
    a <- T.sequence $ write <$> [False, True, True, False] <*> pins'
    print a
