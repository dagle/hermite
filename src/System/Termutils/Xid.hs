module System.Termutils.Xid (
    getXid
    ) where

import Graphics.UI.Gtk

import Foreign
import Foreign.C.Types
import Unsafe.Coerce ( unsafeCoerce )

foreign import ccall "get_xid"
    c_get_xid :: Ptr Window -> CULong

getXid :: Window -> IO Integer
getXid gtkWindow = do
    let ptrWin = unsafeCoerce gtkWindow :: ForeignPtr Window
    withForeignPtr ptrWin $ \realPointer -> do
            return $ fromIntegral $ c_get_xid realPointer
