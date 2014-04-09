{-# LANGUAGE RankNTypes, FlexibleInstances #-}
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
    , defaultHermiteConfig
    , defaultEnv
    , setEnvs
    , loadbindings
    ) where 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import Data.Default

import System.Termutils.Colors
import System.Termutils.Xid
import System.Hermite.Keys

import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.Posix.Env

-- everything with colors doesn't work at all in this module
-- Something goes wrong when interacting with the ffi
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

data HermiteConfig w = HermiteConfig {
    name :: String
    , size :: (Int, Int) -- Width, Height
    --, pos :: (Int, Int) -- maybe a good thing to have?
    , startupHook :: [w -> (HermiteConfig w) -> IO()]
    , terminalHook :: [w -> (HermiteConfig w) -> IO()]
    , events :: [w -> IO ()]
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
    , background = "#ffffffffffff"
    , cursorColor = "#ffffffffffff"
    , highlight = "#2f2f2f"
    , font = "terminus 10"
    , cursorBlink = CursorBlinkSystem
    , cursorShape = CursorShapeBlock 
    , colors = defaultColors
}

loadSettings :: Terminal -> HermiteConfig w -> IO ()
loadSettings terminal cfg' = do
    let cfg = settings cfg'
    terminalSetAllowBold terminal (bold cfg)
    terminalSearchSetWrapAround terminal (searchWrap cfg)
    terminalSetWordChars terminal (wordChars cfg)
    terminalSetScrollbackLines terminal (scrollbackLines cfg)
    terminalSetDeleteBinding terminal (deletebind cfg)
    terminalSetMouseAutohide terminal (hideMouse cfg)

loadTheme :: Terminal -> HermiteConfig w -> IO ()
loadTheme terminal cfg' = do
    let cfg = theme $ settings cfg'
    --terminalSetColors vte (hexToColor . foreground $ htheme) 
    --        (hexToColor . background $ htheme) (colors htheme)
    terminalSetColorBold terminal (hexToColor . foregroundBold $ cfg)
    terminalSetColorDim terminal (hexToColor .foregroundDim $ cfg)
    --terminalSetColorCursor vte (hexToColor . cursorColor $ htheme)
    terminalSetColorHighlight terminal (hexToColor . highlight $ cfg)
    font' <- fontDescriptionFromString (font cfg)
    terminalSetFont terminal font'
    return ()

static :: a -> b -> b1 -> a
static = const . const

gtkThemes :: w -> c -> IO ()
gtkThemes = static $ do
  userGtkConfig <- getUserConfigFile "hermite" "hermite.rc"
  rcSetDefaultFiles [userGtkConfig]


defaultEnv :: Window -> c -> [([Char], IO String, Bool)]
defaultEnv w _ = [
    ("WINDOWID", getXid w >>= return . show, True)
    ,("TERM", return $ "xterm-hermite", True)
    ,("VTE_VERSION", return $ "3405", True)
    ]


setEnvs :: (t -> t1 -> [(String, IO String, Bool)]) -> t -> t1 -> IO ()
setEnvs envs w c = do
    mapM_ (env) $ envs w c
    where env (n,v,b) = do
            v' <- v
            setEnv n v' b

loadbindings :: (HermiteState s, Keybinding a b, Default b) => 
    (w -> (s b) -> [a]) -> w -> Terminal -> IO ()
loadbindings keys w terminal = do
    s <- stateNew def
    _ <- bindkeys (keys w s) terminal s
    return ()
    --bindmouse (mouse w s) terminal s


--hermiteloadConfig :: (HermiteState s, Keybinding a b) => w -> Terminal -> HermiteConfig w (s b) a -> IO ()
hermiteloadConfig :: w -> HermiteConfig w -> IO ()
hermiteloadConfig w cfg = do
    mapM_ (\f -> f w cfg) (terminalHook cfg)

defaultHermiteConfig :: HermiteConfig (Window, Terminal)
defaultHermiteConfig = HermiteConfig {
    name = "xterm-hermite"
    , size = (80,24)
    , startupHook = []
    , terminalHook = []
    , events = []
    , settings = defaultSettings
    , errorMsg = Nothing
}
