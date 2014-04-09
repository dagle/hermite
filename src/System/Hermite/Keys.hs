{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, FlexibleInstances #-}
module System.Hermite.Keys (
    Keybinding(..)
    , Runnable(..)
    , HermiteState(..)
    , NoState
    , swap
    , runKeys
    , setKey
    , bindkeys
    , keyPressed
    ) where 

import Control.Monad.IO.Class
import Control.Monad.State
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk hiding (get)
import Data.IORef

-- A keybinding type that is able to match against a pressed key
-- and a state. If matched, an action is ran mapped to the key.
class (Eq a, Runnable a) => Keybinding a b where
   match :: b -> [Modifier] -> String -> a -> Bool

class Runnable a where
    run :: a -> IO ()
    -- run :: a -> IO b -> IO ()

class HermiteState s where
    stateNew :: a -> IO (s a)
    stateGet :: (s a) -> IO a
    stateWrite :: (s a) -> a -> IO ()

-- A bougus state, for to use when no state is needed
instance HermiteState ((->) ()) where
    stateNew a = return $ const a
    stateGet sa = return $ sa ()
    stateWrite _ _ = return ()

instance HermiteState IORef where
    stateNew = newIORef
    stateGet = readIORef
    stateWrite = writeIORef

-- A dummystate when you don't want a state.
type NoState = ((->) () ())

swap :: Eq a => a -> a -> a
swap k1 k2 = if k1 == k2 then k2 else k1

runKeys :: (Keybinding a b) => [a] -> State [a] () -> [a]
runKeys = flip execState

-- change the keys in a monadic way.
setKey :: (Eq a, MonadState [a] m) => (a -> a -> a) -> a -> m ()
setKey f key' = do
    vec <- get
    let matched = filter ((==) $ key') vec
    case matched of 
        [] -> put $ key' : vec
        _  -> put $ map (f key') vec

-- bindkeys takes a set of bingings, a widget for the bindings and 
-- a starting state.
bindkeys :: (WidgetClass object, HermiteState m, Keybinding a b) =>
                        [a] -> object -> m b -> IO (ConnectId object)
bindkeys bindings vte state' = on vte keyPressEvent $ keyPressed state' bindings

keyPressed :: (HermiteState m, Keybinding a b) => (m b) -> [a] -> EventM EKey Bool
keyPressed b bindings = do
    m <- eventModifier
    key' <- eventKeyName
    b' <- liftIO $ stateGet b
    let matched = filter (match b' m key') bindings
    case matched of
        [] -> return False
        (a:_) -> liftIO (run a) >> return True
