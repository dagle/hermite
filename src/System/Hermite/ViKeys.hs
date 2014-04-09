{-# LANGUAGE MultiParamTypeClasses #-}
module System.Hermite.ViKeys (
    ViMode(..)
    , ViKeyData(..)
    , ViKeys(..)
    , viZero
    , enterCommandMode
    , exitCommandMode
    ) where

import Control.Monad
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Vte.Vte
import System.Hermite.Keys

-- Normal is actually insert mode since insert is more common
data ViMode = ViCmd | ViNormal | ViSelect | ViAll | ViVisual | ViVisualBlock | ViVisualLine

-- Shouldn't we save the selected section independant of vimode?
data ViKeyData = ViKeyData {
    mode :: ViMode
    , beginColumn :: Int
    , beginRow :: Int
    , endColumn :: Int
    , endRow :: Int
}

data ViKeys = ViKeys {
    viMode :: ViMode
    , modifier :: [Modifier]
    , key :: String
    , action :: IO ()
}

instance Eq ViMode where
    ViCmd == ViCmd = True
    ViNormal == ViNormal = True
    ViSelect == ViSelect = True
    _ == ViAll = True
    ViAll == _ = True
    _ == _ = False

instance Eq ViKeys where
    (ViKeys v1 m1 k1 _) == (ViKeys v2 m2 k2 _) =
                (m1 == m2) && (k1 == k2) && (v1 == v2)

instance Runnable ViKeys where 
    run = action

instance Keybinding ViKeys ViKeyData where
    match vi m k (ViKeys vimode modi k' _) = 
        ((mode vi) == vimode) && k == k' && modi == m

instance Default ViKeyData
    def = ViKeyData ViNormal 0 0 0 0

-- this funcion is ran when we enter commandmode and/or change between
-- other modes. It's a convient function that saves the current position so
-- when we change back mode we can get the old position.
updateSelection :: HermiteState s => s ViKeyData -> Terminal -> IO ()
updateSelection vi vte = do
    mode' <- stateGet vi
    terminalSelectNone vte
    when (mode mode' == ViCmd) $ do
        nCol <- terminalGetColumnCount vte
        (col, row) <- terminalGetCursorPosition vte
        terminalSetSelectionBlockMode vte $ mode mode' == ViVisualBlock
        when (mode mode' == ViVisual) $ do
            let begin = beginRow mode' * nCol + beginColumn mode'
                end = row * nCol + col
            if (begin < end )
                then terminalSelectText vte (beginColumn mode') (beginRow mode') col row
                else terminalSelectText vte col row (beginColumn mode') (beginRow mode') 
        when (mode mode' == ViVisualLine) $ do
            terminalSelectText vte 0 (min (beginRow mode') row) (nCol - 1) (max (beginRow mode') row)
        when (mode mode' == ViVisualBlock) $ do
            terminalSelectText vte (min (beginColumn mode') col)
                                   (min (beginRow mode') row) 
                                   (max (beginColumn mode') col) 
                                   (max (beginRow mode') row)
    terminalCopyPrimary vte

-- saves the current position and enters commandmode.
enterCommandMode :: HermiteState s => s ViKeyData -> Terminal -> IO ()
enterCommandMode vi vte = do
    mode' <- stateGet vi
    terminalDisconnectPtyRead vte
    (col, row) <- terminalGetCursorPosition vte
    stateWrite vi $ mode' { mode = ViCmd, beginColumn = col, beginRow = row} 
    updateSelection vi vte

-- leaves commandmode and returns the old position
exitCommandMode:: HermiteState s => s ViKeyData -> Terminal -> IO ()
exitCommandMode vi vte = do
    mode' <- stateGet vi
    terminalSetCursorPosition vte (beginColumn mode') (beginRow mode')
    terminalConnectPtyRead vte
    terminalSelectNone vte
    stateWrite vi $ mode' { mode = ViNormal }
