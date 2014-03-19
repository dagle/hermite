module System.Termutils.Colors (
    runColors
    , setColor
    , hexToColor
    , defaultColor
    , defaultColors
    ) where 

import Control.Monad.State
import Data.Colour
import Data.Word
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.GC
import Data.Bits

-- Run a color configuration in a state monad
runColors :: State [Color] () -> [Color] -> [Color]
runColors = execState

-- set a value value at pos i in a list
set :: Int -> a -> [a] -> [a]
set i v l = 
    let (b, a) = splitAt i l
       in b ++ [v] ++ tail a

-- set color i in monadic form
setColor :: Int -> String -> State [Color] ()
setColor i hex = do
    vec <- get
    let vec' = set i (hexToColor hex) vec
    put vec'

toColor :: (Floating b, RealFrac b) => Colour b -> Color
toColor s = 
    let (RGB r g b) = toSRGBBounded s :: RGB Word16
        in (Color r g b)

hexToColor :: String -> Color
hexToColor str = 
    let colour = sRGB24read str
        in toColor colour

color16 :: Int -> Int -> Double
color16 i x = 
    let hi = if (i .&. x) == 0 then 49152.0 else 0.0
        lo = if i > 7 then 16383.0 else 0.0
        in (hi + lo) / 65535.0

color232 :: Int -> Double
color232 c =
    part $ if c == 0 then 0 else c * 40 + 55

part :: Int -> Double 
part col = fromIntegral (col .|. shift col 8) / 65535.0

-- get the defaultColor, only defined values between 0 and 255
defaultColor :: Int -> Color
defaultColor i
    | i >= 0 && i < 16 = 
        let blue = color16 i 4
            green = color16 i 2
            red = color16 i 1
            in toColor $ rgb red green blue
    | i >= 0 && i < 232 =
        let i = i - 16
            r = div i 36
            g = (div i 6) `mod` 6 
            b = mod i 6
            red = color232 r
            green = color232 g
            blue = color232 b
            in toColor $ rgb red green blue
    | i >= 0 && i < 256 =
        let shade = 8 + (i - 232) * 10
            red = part shade
            green = red
            blue = red
            in toColor $ rgb red green blue

defaultColors :: [Color]
defaultColors = map defaultColor [0..255]
