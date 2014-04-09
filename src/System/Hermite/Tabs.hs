module System.Hermite.Tabs (
    tabNew
    , liftTermIndex
    , liftTerm
    , changeTitleTab
    , deleteTab
    , tabDeleteCurrent
    , tmap
    , hermiteTabNew
    , configTab
    --, module Graphics.UI.Gtk.Layout.Notebook
    ) where 

import Control.Monad
import Graphics.UI.Gtk.Layout.Notebook
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import System.Hermite.Settings

castToTerminal :: (WidgetClass o) => o -> Terminal 
castToTerminal = undefined

hermiteTabNew :: IO (Window, Notebook)
hermiteTabNew = do
    window <- windowNew
    note <- notebookNew
    return (window, note)

configTab :: (Window, Notebook) -> HermiteConfig (Window, Notebook) -> IO ()
configTab w@(window, note) cfg = do
    containerAdd window note
    _ <- onDestroy window mainQuit
    uncurry (widgetSetSizeRequest window) $ size cfg
    tabNew w note cfg
    widgetShowAll window

tmap :: NotebookClass self => (Terminal -> IO b) -> self -> IO [b]
tmap f note = do
    n <- notebookGetNPages note 
    mapM (\i -> liftTermIndex f i note) [0..n]

tabNew :: w -> Notebook -> HermiteConfig w -> IO ()
tabNew w note cfg = do
    vte <- terminalNew
    hermiteloadConfig w cfg
    _ <- on vte childExited $ deleteTab note vte
    _ <- on vte windowTitleChanged $ changeTitleTab note vte
    _ <- notebookAppendPage note vte "Terminal"
    _ <- terminalForkCommand vte Nothing Nothing Nothing Nothing False False False
    widgetGrabFocus vte
    return ()

-- unsafe, should have a default case
liftTermIndex :: NotebookClass self => (Terminal -> IO b) 
        -> Int -> self -> IO b
liftTermIndex f i note = do 
    w <- notebookGetNthPage note i
    case w of
        (Just w') -> f (castToTerminal w')
        Nothing -> do
                Just w' <- notebookGetNthPage note 0
                f (castToTerminal w')

liftTerm :: NotebookClass self => (Terminal -> IO b) -> self -> IO b
liftTerm f note = do
    i <- notebookGetCurrentPage note
    liftTermIndex f i note

changeTitleTab :: (NotebookClass self) => self -> Terminal -> IO ()
changeTitleTab note vte =
    terminalGetWindowTitle vte >>= notebookSetTabLabelText note vte

tabDeleteCurrent :: NotebookClass self => self -> IO ()
tabDeleteCurrent note = 
    notebookGetCurrentPage note >>= notebookRemovePage note

deleteTab :: (WidgetClass w, NotebookClass self) => self -> w -> IO ()
deleteTab note vte = do
    num <- notebookGetNPages note
    when (num == 1) mainQuit
    i <- notebookPageNum note vte
    case i of
        Nothing -> return ()
        (Just idx) -> notebookRemovePage note idx
