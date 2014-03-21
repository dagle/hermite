{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module System.Hermite.SimpleKeys (
    Keybinding(..)
    , Runnable(..)
    , SimpleKeys(..)
    , runKeys
    , swap
    , setKey
    , bindkeys
    , toKeybind
    , defaultKeys
    , keyPressed
    ) where 

import Control.Monad.IO.Class
import Control.Monad.State
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk hiding (get)

class (Eq a, Runnable a) => Keybinding a b where
   fromSimpleKeys :: SimpleKeys -> a
   match :: b -> [Modifier] -> String -> a -> Bool

class Runnable a where
    run :: a -> IO ()

data SimpleKeys = SimpleKeys {
    modifier :: [Modifier]
    , key :: String
    , action :: IO ()
}

instance Keybinding SimpleKeys () where
    fromSimpleKeys = id
    match _ modi k (SimpleKeys modi' k' _) =
        (modi == modi') && (k == k')

instance Runnable SimpleKeys where
    run = action

instance Eq SimpleKeys where
    (SimpleKeys m1 k1 _) == (SimpleKeys m2 k2 _) = 
                (m1 == m2) && (k1 == k2)

-- Run a color configuration in a state monad
runKeys :: (Keybinding a b) => [a] -> State [a] () -> [a]
runKeys = flip execState

swap :: Eq a => a -> a -> a
swap k1 k2 = if k1 == k2 then k2 else k1

setKey :: (Eq a, MonadState [a] m) => (a -> a -> a) -> a -> m ()
setKey f key' = do
    vec <- get
    let matched = filter ((==) $ key') vec
    case matched of 
        [] -> put $ key' : vec
        _  -> put $ map (f key') vec


bindkeys :: (WidgetClass object, Keybinding a b) =>
            [a] -> object -> IO b -> IO (ConnectId object)
bindkeys bindings vte b = on vte keyPressEvent $ keyPressed b bindings

toKeybind :: ([Modifier], String, IO ()) -> SimpleKeys
toKeybind (m, k, a) = SimpleKeys m k a

defaultKeys :: [SimpleKeys]
defaultKeys = map toKeybind [
    ([Control],  "p",  return ())
    ]

-- runs the first action found 
keyPressed :: (Keybinding a b) => IO b -> [a] -> EventM EKey Bool
keyPressed b bindings = do
    m <- eventModifier
    key' <- eventKeyName
    b' <- liftIO b
    let matched = filter (match b' m key') bindings
    case matched of
        [] -> return False
        (a:_) -> liftIO (run a) >> return True
