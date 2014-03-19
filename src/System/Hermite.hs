-- | Main module of Hermite
module System.Hermite (
    -- * Detail
    --
    -- | This is a lightweight system terminal for people that want full control
    -- of their terminal editor without writing their own. It follows the spirit
    -- xmonad but use dyre to support dynamic reconfiguration. Everything can be
    -- changed, from default colors, how urls are handled, to creating popups on
    -- events. Hermite uses gnomes VTE internaly, like many other terminal,
    -- because of it's good bindings and standard support.
    --
    -- This is the real main module and should not be modified.
    -- For costumization Hermite (and dyre) edit the config file
    -- (~/.config/hermite/hermite.hs). Typically this means just setting colors
    -- and edit bindings and adding 1 or 2 widgets, but you could change all
    -- of the behaviour of hermite if wanted. All you really are forced to do is
    -- to spawn a vte and a window containing that vte.
    --
    -- * Config File
    --
    -- The config file is just plain haskell that is compiled (if changed)
    -- at startup to create a custom terminal. You import this and it's helper
    -- functions and you are ready to create your own terminal.
    --
    HermiteConfig(..),
    defaultHermite,
    defaultHermiteConfig,
    termiteMain
    ) where

import qualified Config.Dyre as Dyre
import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.FilePath ( (</>) )
import Graphics.UI.Gtk

showError :: HermiteConfig -> String -> HermiteConfig
showError cfg msg = cfg { errorMsg = Just msg }

-- | The default parameters need to tell GHC to compile using
-- -threaded so that the GTK event loops doesn't block all of the
-- widgets
defaultParams :: Dyre.Params TaffybarConfig
defaultParams = Dyre.defaultParams { Dyre.projectName = "hermite"
                                   , Dyre.realMain = realMain
                                   , Dyre.showError = showError
                                   , Dyre.ghcOpts = ["-threaded"]
                                   }
defaultHermite :: TaffybarConfig -> IO ()
defaultHermite = Dyre.wrapMain defaultParams

realMain :: HermiteConfig -> IO ()
realMain cfg = do
    case errorMsg cfg of
        Nothing -> hermiteMain cfg
        Just err -> error ("Error: " ++ err)

getDefaultConfigFile :: String -> IO FilePath
getDefaultConfigFile name = do
  dataDir <- getDataDir
  return (dataDir </> name)

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
                    liftIO action
                    return True


hermiteMain :: HermiteConfig-> IO ()
hermiteMain cfg = do
  -- Override the default GTK theme path settings.  This causes the
  -- terminal (by design) to ignore the real GTK theme and just use the
  -- provided minimal theme to set, this does not effect the vte widget
  -- only the gtk windows.
  -- Users can override this default.
  defaultGtkConfig <- getDefaultConfigFile "hermite.rc"
  userGtkConfig <- getUserConfigFile "hermite" "hermite.rc"
  rcSetDefaultFiles [ defaultGtkConfig, userGtkConfig ]

  _ <- initGUI
  window <- windowNew
  vte <- terminalNew
  _ <- on vte childExited mainQuit

  xid <- windowXid(window)
  setEnv "WINDOWID" (show xid) True
  setEnv "TERM" (name cfg)True
  setEnv "VTE_VERSION" "3405" True

  -- keys are always bound and does not need to be in the eventlist
  let events' = (bindkeys (keybinding cfg)) : events cfg
  _ <- sequence  map (\event -> event vte window) events'

  -- setup events
  -- setup keybindings
  terminalForkCommand vte Nothing Nothing Nothing Nothing False False False
  containerAdd window vte
  -- lets do this before we start parsing, so no zombies get left behind
  uncurry (widgetSetSizeRequest window) $ size cfg
  widgetShowAll window

  mainGUI

data HermiteConfig = HermiteConfig {
    name :: String
    , size :: (Int, Int) -- Width, Height
    --, pos :: (Int, Int) -- maybe a good thing to have?
    , keybindings :: [Keybinding]
    , events :: (WidgetClass w) => [Terminal -> Window -> IO (ConnectId w)]
    , settings :: Settings
    , errorMsg :: Maybe String
}

defaultHermiteConfig = HermiteConfig {
    name = "xterm-hermite"
    , size = (80,24)
    , keybindings = defaultKeybindings
    , events = defaultEvents
    , settings = defaultSettings
    , errorMsg = Nothing
}

-- these things are the only things hermite keeps track of
-- other windows, widgets, etc need to kept track manually
-- by the callback, bindkey and it's own data.
data Runtime = Runtime {
    conf :: HermiteConfig
    vte :: Terminal
    mainWindow :: Window
--    callbackIds :: [(ConnectId (Either Terminal Window)]
}

