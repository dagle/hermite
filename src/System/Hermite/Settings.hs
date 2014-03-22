module System.Hermite.Settings (
    HermiteConfig(..)
    , Theme(..)
    , HermiteSettings(..)
    , loadTheme
    , loadSettings
    , hermiteloadConfig
    , defaultTheme
    , defaultSettings
    , gtkThemes
    ) where 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte

import System.Termutils.Colors
import System.Hermite.SimpleKeys

import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.FilePath ( (</>) )
import System.Posix.Env

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

data HermiteConfig = HermiteConfig {
    name :: String
    , size :: (Int, Int) -- Width, Height
    --, pos :: (Int, Int) -- maybe a good thing to have?
    , keybindings :: [SimpleKeys]
    , events :: [Terminal -> Window -> IO ()]
    --, events :: (WidgetClass w) => [Terminal -> Window -> IO (ConnectId w)]
    , settings :: HermiteSettings
    , errorMsg :: Maybe String
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

gtkThemes :: IO ()
gtkThemes = do
  userGtkConfig <- getUserConfigFile "hermite" "hermite.rc"
  rcSetDefaultFiles [userGtkConfig]

-- takes a vte and loads setting (theme, bindings etc etc) into it. 
hermiteloadConfig :: Terminal -> HermiteConfig -> IO ()
hermiteloadConfig terminal cfg = do

  --xid <- windowXid(window)
  --setEnv "WINDOWID" (show xid) True
  setEnv "TERM" (name cfg) True
  setEnv "VTE_VERSION" "3405" True

  -- keys are always bound and does not need to be in the eventlist
  _ <- bindkeys (keybindings cfg) terminal (return ())

  loadSettings (settings cfg) terminal
  loadTheme (theme $ settings cfg) terminal
  _ <- terminalForkCommand terminal Nothing Nothing Nothing Nothing False False False
  return ()
