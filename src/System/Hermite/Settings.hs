module System.Hermite.Settings (
    Theme(..)
    , HermiteSettings(..)
    , loadTheme
    , loadSettings
    , defaultTheme
    , defaultSettings
    )
import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk.Abstract.Widget (Color)
import System.Termutil.Colors (defaultColors)

data HermiteSettings = HermiteSettings {
    bold :: Bool
    , searchWrap :: Bool
    , hideMouse :: Bool
    , wordChars :: String
    , lines :: Int
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

defaultSettings = HermiteSettings {
    bold = True
    , searchWrap = True
    , hideMouse = True
    , wordChars = "-A-Za-z0-9,./?%&#:_=+@~"
    , lines = 100
    , deletebind = VteEraseAuto
    , theme = defaultTheme
}

defaultTheme = Theme {
    foreground = "#dcdccc"
    , foregroundBold = "#ffffff"
    , foregroundDim = "#888888"
    , background = "#3f3f3f"
    , cursorColor = "#dcdccc"
    , highlight = "#2f2f2f"
    , font = "Monospace 12"
    , cursorBlink = VteCursorBlinkSystem
    , cursorShape = VteCursorShapeBlock 
    , colors = defaultColors
}

loadSettings :: HermiteSettings -> IO ()
loadSettings settings = do
    terminalSetAllowBold vte (bold settings)
    terminalSearchSetWrapAround vte (searchWrap settings)
    terminalSetWordChars vte (wordChars settings)
    terminalSetScrollbackLines vte (lines settings)
    terminalSetDeleteBinding vte (deletebind settings)
    terminalSetMouseAutohide vte (hideMouse settings)

loadTheme :: Theme -> IO ()
loadTheme htheme = do
    terminalSetColors vte (foreground htheme) 
            (background $ htheme) (colors htheme)
    terminalSetColorBold vte (foregroundBold htheme)
    terminalSetColorDim vte (foregroundDim htheme)
    terminalSetColorCursor vte (cursor htheme)
    terminalSetColorHighlight vte (highlight htheme)
    font' <- fontDescriptionFromString (font htheme)
    terminalSetFont vte font'
