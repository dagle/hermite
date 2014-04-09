{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ConstraintKinds #-}
{- 
 - A set of simple bindings, no modes, no cords, just a modifier and a key.
 -}
module System.Hermite.SimpleKeys (
    Keybinding(..)
    , Runnable(..)
    , SimpleKeysT(..)
    , runKeys
    , swap
    , setKey
    , bindkeys
    , toKeybind
    , defaultKeys
    , keyPressed
    ) where 

import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk hiding (get)
import System.Hermite.Keys

data SimpleKeysT = SimpleKeys {
    modifier :: [Modifier]
    , key :: String
    , action :: IO ()
}

instance Keybinding SimpleKeysT () where
    match _ modi k (SimpleKeys modi' k' _) =
        (modi == modi') && (k == k')

instance Runnable SimpleKeysT where
    run = action

instance Eq SimpleKeysT where
    (SimpleKeys m1 k1 _) == (SimpleKeys m2 k2 _) = 
                (m1 == m2) && (k1 == k2)

-- Convient functions that lets you write bindings without
-- typing constructors.
toKeybind :: ([Modifier], String, IO ()) -> SimpleKeysT
toKeybind (m, k, a) = SimpleKeys m k a

-- Default bindings for a simple terminal
defaultKeys :: (Keybinding SimpleKeysT ()) => (Window, Terminal) -> IO () -> [SimpleKeysT]
defaultKeys (w, t) _ = map toKeybind [
    ([Control],  "x", return ())
    ]
