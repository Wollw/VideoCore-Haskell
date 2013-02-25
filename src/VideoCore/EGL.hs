{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.EGL where

import Foreign
import Foreign.C

type Boolean = CUInt
type Enum = CUInt
type Display = Ptr ()
type NativeDisplayType = Ptr ()

foreign import ccall unsafe "eglGetDisplay" getDisplay :: NativeDisplayType -> IO Display
