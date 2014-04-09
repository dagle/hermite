module System.Termutils.Colors (
    runColors
    , setColor
    , hexToColor
    , defaultColor
    , defaultColors
    ) where 

import Control.Monad.State
import Data.Char
import Data.Word
import Data.Bits
import Graphics.UI.Gtk hiding (get, set)


-- Run a color configuration in a state monad
runColors :: [Color] -> State [Color] () -> [Color]
runColors = flip execState

-- set a value value at pos i in a list
set :: Int -> a -> [a] -> [a]
set i v l = 
    let (b, a) = splitAt i l
       in b ++ [v] ++ tail a

-- set color i in monadic form
setColor :: Int -> String -> State [Color] ()
setColor i hex = do
    vec <- get
    put $ set i (hexToColor hex) vec

hexToColor :: String -> Color
hexToColor ('#':a:b:c:d:e:g:_)  = Color (f a b) (f c d) (f e g)
    where f x y = fromIntegral $ (digitToInt x * 0x10 + digitToInt y) * 0x100
hexToColor ('#':a:b:c:_)    = Color (f a) (f b) (f c)
    where f x = fromIntegral $ digitToInt x * 0x1000
hexToColor _            = Color 0 0 0

color16 :: Int -> Int -> Word16
color16 i x = 
    let hi = if (i .&. x) == 0 then 49152 else 0
        lo = if i > 7 then 16383 else 0
        in (hi + lo)

color232 :: Int -> Word16
color232 c =
    part $ if c == 0 then 0 else c * 40 + 55

part :: Int -> Word16
part col = fromIntegral (col .|. shift col 8) 

-- get the defaultColor, only defined values between 0 and 255
--
defaultColor :: Int -> Color
defaultColor i
    | i >= 0 && i < 16 = 
        let blue = color16 i 4
            green = color16 i 2
            red = color16 i 1
            in Color red green blue
    | i >= 0 && i < 232 =
        let j = j - 16
            r = div j 36
            g = (div j 6) `mod` 6 
            b = mod j 6
            red = color232 r
            green = color232 g
            blue = color232 b
            in Color red green blue
    | i >= 0 && i < 256 =
        let shade = 8 + (i - 232) * 10
            red = part shade
            green = red
            blue = red
            in Color red green blue
    | True = Color 0 0 0

defaultColors :: [Color]
defaultColors = map defaultColor [0..255]
