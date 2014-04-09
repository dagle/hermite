module System.Termutils.Search (
    findNext
    , findPrev
    , search
    , searchForward
    , searchBackward
    ) where

import Graphics.UI.Gtk.Vte.Vte

findNext terminal = do
    terminalSearchFindNext terminal
    terminalCopyPrimary terminal

findPrev terminal = do
    terminalSearchFindPrevious terminal
    terminalCopyPrimary terminal


search = undefined
{- 
search terminal str reverse = do
    let func = if reverse
                  then terminalSearchFindPrevious
                  else terminalSearchFindNext
    r <- terimanlSearchGetGrex terminal
}
-}

searchForward terminal str = search terminal str False
searchBackward terminal str = search terminal str True 
