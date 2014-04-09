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
    -- to spawn a terminal and a window containing that vte.
    --
    -- * Config File
    --
    -- The config file is just plain haskell that is compiled (if changed)
    -- at startup to create a custom terminal. You import this and it's helper
    -- functions and you are ready to create your own terminal.
    --
    HermiteConfig(..)
    , defaultHermite
    , defaultHermiteConfig
    , hermiteMain
    ) where

import qualified Config.Dyre as Dyre
import Graphics.UI.Gtk
import System.Hermite.Settings
--import Graphics.UI.Gtk.Vte.Vte


showError :: HermiteConfig w -> String -> HermiteConfig w
showError cfg msg = cfg { errorMsg = Just msg }

-- | The default parameters need to tell GHC to compile using
-- -threaded so that the GTK event loops doesn't block all of the
-- widgets
defaultParams :: Dyre.Params (HermiteConfig w)
defaultParams = Dyre.defaultParams { Dyre.projectName = "hermite"
                                   , Dyre.realMain = realMain
                                   , Dyre.showError = showError
                                   , Dyre.ghcOpts = ["-threaded"]
                                   }
defaultHermite :: HermiteConfig w -> IO ()
defaultHermite = Dyre.wrapMain defaultParams

realMain :: HermiteConfig w -> IO ()
realMain cfg = do
    case errorMsg cfg of
        Nothing -> hermiteMain cfg
        Just err -> error ("Error: " ++ err)

-- Standard hermiteMain for users only needing a standard terminal
hermiteMain :: HermiteConfig w -> IO ()
hermiteMain cfg = do
  gtkThemes () () -- really? 
  _ <- initGUI
{-
  window <- windowNew
  terminal <- terminalNew
  _ <- on terminal childExited mainQuit
  _ <- onDestroy window mainQuit
  containerAdd window terminal -}
--  _ <- uncurry (widgetSetSizeRequest window) $ size cfg
--  widgetModifyBg terminal StateNormal $ hexToColor $ background $ theme $ settings cfg
--  widgetShowAll window

  mainGUI
-- these things are the only things hermite keeps track of
-- other windows, widgets, etc need to kept track manually
-- by the callback, bindkey and it's own data.
--data Runtime = Runtime {
--    conf :: HermiteConfig
--    , vte :: Terminal
--    , mainWindow :: Window
--    , callbackIds :: WidgetClass object => [(ConnectId object)]
