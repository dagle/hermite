module System.Termutil.Colors (
    runColors
    , setColor
    , hexToColor
    , defaultColor
    , defaultColors
    ) where 

import Control.Monad.State
import Data.Colour

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
setColor i hex =
    vec <- get
    let vec' = set i (hexToColor hex) vec
    put vec'

toColor :: Colour -> Color
toColor s = 
    let (RGB r g b) = toSRGBBounded s :: RGB Word16
        (Color r g b)

hexToColor :: String -> Color
hexToColor str = 
    let colour = sRGB24read str
        toColor colour

color16 :: Int -> Int -> Double
color16 i x = 
    let hi = if (i .&. x) == 0 then 49152.0 else 0.0
        lo = if i > 7 then 16383.0 else 0.0
        in (hi + low) / 65535.0

color232 :: Int -> Double
color232 c =
    part $ if c == 0 then 0 else c * 40 + 55

part col :: Int -> Double 
part col = (col .|. shift col 8) / 65535

-- get the defaultColor, only defined values between 0 and 255
defaultColor :: Int -> Color
defaultColor i
    | i >= 0 && i < 16 = 
        let blue = color16 i 4
            green = color16 i 2
            red = color16 i 1
            toColor $ rgb red green blue
    | i >= 0 && i < 232 =
        let i = i - 16
            r = div i 36
            g = (div i 6) `mod` 6 
            b = mod i 6
            red = color232 r
            green = color232 g
            blue = color232 b
            toColor $ rgb red green blue
    | i >= 0 && i < 256 =
        let shade = 8 + (i - 232) * 10
            red = part shade
            green = red
            blue = red
            toColor $ rgb red green blue

defaultColors :: [Color]
defaultColors = map defaultColor [0..255]
