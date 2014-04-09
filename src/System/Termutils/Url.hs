module System.Termutils.Url (
    findUrls
    , matchUrls
--    , showUrls
    , launchUrl
) where

import Text.Regex
import Graphics.UI.Gtk.Vte.Vte
import System.Posix.Process
import Control.Concurrent
import Codec.Binary.UTF8.String (encodeString)

{-
data Attr = Attr {
    column :: Int
    , row :: Int
    , foreground :: Color
    , background :: Color
    , underline :: Bool
    , strikeThrough :: Bool
}
-}

findUrls :: Terminal -> String -> IO [(VteChar, VteChar, String)]
findUrls vte regex = do
    let regx = mkRegex regex
    vteChars <- terminalGetText vte Nothing
    return $ matchUrls regx vteChars 0

--fetchAttr = ffi
matchUrls :: Regex -> [VteChar] -> Int -> [(VteChar, VteChar, String)]
matchUrls regex content i = 
    let str = map vcChar content
    in case matchRegexAll regex str of
        Nothing -> []
        (Just (b, m, _, _)) -> 
            let pos = i + length b
                posAfter = pos + length m
                in (content !! pos, content !! posAfter, m) : matchUrls regex (drop posAfter content) posAfter

--showUrls :: (Input a) => Terminal -> a -> IO ()
--showUrls term a = do
--    attrs <- findUrls term "http:"
--    selector a attrs 
--    showInput a attrs launchUrl
--    hideInput a
--
spawn :: String -> IO ()
spawn x = spawnPID x >> return ()

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: String -> IO ThreadId
spawnPID x = forkIO $ executeFile "/bin/sh" False ["-c", encodeString x] Nothing


launchUrl :: String -> IO ()
launchUrl str = spawn $ "firefox " ++ str

--scrolled :: Int -> [(Attr, Attr, String)] -> [(Attr, Attr, String)]
-- More helper functions etc.  
-- when widgets are done (should  be located in widgets)
