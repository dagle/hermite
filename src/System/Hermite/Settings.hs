module System.Hermite.Settings (
    Theme(..)
    , HermiteSettings(..)
    , loadTheme
    , loadSettings
    , defaultTheme
    , defaultSettings
    ) where 

import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk.Abstract.Widget (Color)
import Graphics.Rendering.Pango.Font
import System.Termutils.Colors

data HermiteSettings = HermiteSettings {
    bold :: Bool
    , searchWrap :: Bool
    , hideMouse :: Bool
    , wordChars :: String
    , scrollbackLines :: Int
    , deletebind :: TerminalEraseBinding
    , theme :: Theme
}

data Theme = Theme {
    foreground :: String
    , foregroundBold :: String
    , foregroundDim :: String
    , background :: String
    , cursorColor :: String
    , highlight :: String
    , font :: String
    , cursorBlink :: TerminalCursorBlinkMode
    , cursorShape :: TerminalCursorShape
    , colors :: [Color]
}
defaultSettings :: HermiteSettings
defaultSettings = HermiteSettings {
    bold = True
    , searchWrap = True
    , hideMouse = True
    , wordChars = "-A-Za-z0-9,./?%&#:_=+@~"
    , scrollbackLines = 100
    , deletebind = EraseAuto
    , theme = defaultTheme
}
defaultTheme :: Theme
defaultTheme = Theme {
    foreground = "#dcdccc"
    , foregroundBold = "#ffffff"
    , foregroundDim = "#888888"
    , background = "#3f3f3f"
    , cursorColor = "#dcdccc"
    , highlight = "#2f2f2f"
    , font = "Monospace 12"
    , cursorBlink = CursorBlinkSystem
    , cursorShape = CursorShapeBlock 
    , colors = defaultColors
}

loadSettings :: HermiteSettings -> Terminal -> IO ()
loadSettings settings vte = do
    terminalSetAllowBold vte (bold settings)
    --terminalSearchSetWrapAround vte (searchWrap settings)
    terminalSetWordChars vte (wordChars settings)
    terminalSetScrollbackLines vte (scrollbackLines settings)
    terminalSetDeleteBinding vte (deletebind settings)
    terminalSetMouseAutohide vte (hideMouse settings)

loadTheme :: Theme -> Terminal -> IO ()
loadTheme htheme vte = do
    --terminalSetColors vte (foreground htheme) 
    --        (background $ htheme) (colors htheme)
    terminalSetColorBold vte (hexToColor . foregroundBold $ htheme)
    terminalSetColorDim vte (hexToColor .foregroundDim $ htheme)
    terminalSetColorCursor vte (hexToColor . cursorColor $ htheme)
    terminalSetColorHighlight vte (hexToColor . highlight $ htheme)
    font' <- fontDescriptionFromString (font htheme)
    terminalSetFont vte font'
