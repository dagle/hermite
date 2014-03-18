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
import Text.Printf


