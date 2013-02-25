module VideoCore.EGL.Macros where

import qualified VideoCore.EGL as EGL
import Foreign

defaultDisplay :: EGL.NativeDisplayType
defaultDisplay = intPtrToPtr 0
