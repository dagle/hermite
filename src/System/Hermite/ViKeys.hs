module System.Hermite.ViKeys (
    ViMode(..)
    , ViKeyData(..)
    , ViKeys(..)
    , enterCommandMode
    , exitCommandMode
    )

-- Normal is actually insert mode since insert is more common
data ViMode = ViCmd | ViNormal | ViSelect | ViAll

data ViKeyData = ViKeyData {
    mode :: ViMode
    , column :: Int
    , row :: Int
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

instance Keybinding ViKeys b where
    fromSimpleKeys (SimpleKeys m k a)  = ViKeys ViNormal m k a
    match vi (ViKeys mode modi k' a) m k = 
        | (mode vi) != mode = False
        | k == k' && modi == m = True 
        | _ = False

-- should figure out a better way to pass around arguments
-- without makeing them less generic or strings or horrible to use
-- (aka pain in the but)
updateSelection vte vi = do
    terminalSelectNone vte
    when (mode vi == ViCmd) $ do
        let nCol = terminalGetColumnCount vte
            (col, row) = terminalGetCursorPosition vte
        terminalSetSelectionBlockMode vte $ mode vi == ViVisual
        when (mode vi == ViVisual) $ do
            let begin =
                end =
            if (begin < end )
                then terminalSelectText vte 
                else terminalSelectText vte 
        when (mode vi == ViVisualLine) $ do
            terminalSelectText vte 
        when (mode vi == ViVisualBlock) $ do
            terminalSelectText vte 
    terminalCopyPrimary vte

enterCommandMode :: 
enterCommandMode vi vte = do
    terminalDisconnecPtyRead vte
    (col, row') <- terminalGetCursorPosition vte
    updateSelection vte vi
    put vi $ mode { mode = ViCmd, column = col, row = row'} 

exitCommandMode ::
exitCommandMode vi vte = do
    mode <- vi
    terminalSetCursorPosition vte (column mode) (row mode)
    terminalConnectPtyRead vte
    terminalSelectNone vte
    put vi $ mode { mode = ViNormal }
