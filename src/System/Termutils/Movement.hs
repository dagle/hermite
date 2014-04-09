module System.Termutils.Movement (
    moveColumn
    , modifyColumn
    , moveRow
    , modifyRow
    , moveForward
    , moveBackward
    , moveEol
    ) where 
-- Most of these functions only work if the term is in a special state
-- use with care 

import Graphics.UI.Gtk.Vte.Vte

moveColumn :: Terminal -> Int -> IO ()
moveColumn vte c = modifyColumn vte (const c)

modifyColumn :: Terminal -> (Int -> Int) -> IO ()
modifyColumn vte f = do
    (col, row) <- terminalGetCursorPosition vte
    terminalSetCursorPosition vte (f col) row
    -- update_selection(info->vte, &info->select); butt whipe? 

moveRow :: Terminal -> Int -> IO ()
moveRow vte r = modifyRow vte (const r)

modifyRow :: Terminal -> (Int -> Int) -> IO ()
modifyRow vte f = do
    (col, row) <- terminalGetCursorPosition vte
    terminalSetCursorPosition vte col (f row)

-- Not the best code but works(tm) (also less buggy than termite)
moveBackward :: Terminal -> t -> IO ()
moveBackward vte isWord = do
    (col, row) <- terminalGetCursorPosition vte
    moveB vte isWord row 0 row col []
    -- updateSelection

moveForward :: Terminal -> t -> IO ()
moveForward vte isWord = do
    (col, row) <- terminalGetCursorPosition vte
    endCol <- terminalGetColumnCount vte
    moveF vte isWord row col row endCol []

moveF :: Terminal -> t -> Int -> Int -> Int -> Int -> [Int] -> IO ()
moveF vte isWord startRow startCol endRow endCol rows = do
    -- we need to get the max pos
    cont <- terminalGetTextRange vte startRow startCol endRow endCol Nothing
    let rcont = reverse cont
        rows' = length cont : rows 
    case getDistance rcont isWord rows' of
       (Just i) -> terminalSetCursorPosition vte (endCol-i) startRow
       Nothing -> moveB vte isWord (startRow-1) startCol endRow endCol rows'

moveB :: Terminal -> t -> Int -> Int -> Int -> Int -> [Int] -> IO ()
moveB vte isWord startRow startCol endRow endCol rows = do
    -- we need to get the max pos
    cont <- terminalGetTextRange vte startRow startCol endRow endCol Nothing
    let rcont = reverse cont
        rows' = rows ++ [length cont]
    case getDistance rcont isWord rows' of
       (Just i) -> terminalSetCursorPosition vte (endCol+i) startRow
       Nothing -> do
           endCol' <- terminalGetColumnCount vte
           moveB vte isWord startRow startCol (endRow+1) endCol' rows'

getDistance = undefined
--moveEol vte = moveForward vte eol 
moveEol = undefined
{-
void move_backward_blank_word(keybind_info *info, GdkEventKey *key, void *data) {
    move_backward(info->vte, &info->select, std::not1(std::ref(g_unichar_isspace)));
}
void move_forward_blank_word(keybind_info *info, GdkEventKey *key, void *data) {
    move_forward(info->vte, &info->select, std::not1(std::ref(g_unichar_isspace)));
}
void move_backward_word(keybind_info *info, GdkEventKey *key, void *data) {
    move_backward(info->vte, &info->select, std::bind(vte_terminal_is_word_char, info->vte, _1));
}
void move_forward_word(keybind_info *info, GdkEventKey *key, void *data) {
    move_forward(info->vte, &info->select, std::bind(vte_terminal_is_word_char, info->vte, _1));
}


void move(keybind_info *info, GdkEventKey *, void *data) {
    /* get col and rows */
    int col, row;
    const long end_col = vte_terminal_get_column_count(info->vte) - 1;

    long cursor_col, cursor_row;
    vte_terminal_get_cursor_position(info->vte, &cursor_col, &cursor_row);

    vte_terminal_set_cursor_position(info->vte,
                                     clamp(cursor_col + col, 0l, end_col),
                                     clamp(cursor_row + row, first_row(info->vte), last_row(info->vte)));

    update_scroll(info->vte);
    update_selection(info->vte, &info->select);
}
-}

