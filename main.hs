module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import Graphics.Rendering.Pango.Font
import System.Posix.Env
import Text.Read

term = "xterm-hermite"
-- look into the speed of all
-- this, maybe we want to do
-- some template haskell to
-- generate static stuff from
-- dynamic.

data Hermite a = Hermite {
    name :: String
    , parseArgs :: [String] -> a
    , startupHooks :: IO ()
    , keybindings :: [Keybinding]
    , events :: [IO (ConnectId obj)]
    , config :: a
}

data Keybinding = Keybinding {
    modifier :: [Modifier]
    , key :: String
    , action :: IO ()
}

defaultConfig = Hermite {
    name = "xterm-hermite"
     parseArg = const mempty
    , startupHooks = defaultStartup
    , keybindings = defaultKeys
    , events = defaultsEvents
    , config = mempty
}

defaultEvents = [
    onDestroy
    , windowState
    , urgentBeep
    , button_press
    , getFocus
    , loseFocus
    , bindkeys
    ]
    
defaultStartup = [
    setEnv
    , loadTheme
    , loadEvents
    ]

realStartup = do
    initGUI
    window <- windowNew
    vte <- terminalNew
    sequence defaultStartup

-- HOW THE FUCK DO I PASS vte, window and any user
-- supplied windows?



--bindkeys :: [Keybinding] -> IO (ConnectId obj)
bindkeys bindings vte = on vte keyPressEvent $ keyPressed bindings

match mod key (Keybinding (mod', key', _)) =
    (mod == mod') && (key == key')

keyPressed bindings = do
    m <- eventModifier
    key <- eventKeyName
    let matched = filter (match m key) bindings 
    case matched of 
        [] -> return False
        ((Keybinding (_, _, action)):xs) -> 
                    liftIO $ action
                    return True

main = do
    initGUI
    window <- windowNew
    vte <- terminalNew

    -- GDK_WINDOW_XID mapping in utils
    -- setEnv "WINDOWID" (show xid) TRUE
    setEnv "TERM" term True
    setEnv "VTE_VERSION" "3405" True

    terminalForkCommand vte Nothing Nothing Nothing Nothing False False False
    font <- fontDescriptionFromString "DejaVu Sans Mono 10"
    terminalSetFont vte font
    containerAdd window vte
    on vte childExited $ mainQuit
    widgetSetSizeRequest window 640 480

    widgetShowAll window

    mainGUI
