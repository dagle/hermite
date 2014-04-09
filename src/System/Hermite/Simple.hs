hermiteSimpleNew :: IO (Window, Terminal)
hermiteSimpleNew cfg = do
    _ <- initGUI
    window <- windowNew
    terminal <- terminalNew
    return $ (window, terminal)

configSimple (window, term) cfg = do
    _ <- on terminal childExited mainQuit
    _ <- onDestroy window mainQuit
    uncurry (widgetSetSizeRequest window) $ size cfg
    containerAdd window terminal
    hermiteloadConfig terminal cfg
    widgetShowAll window
