module VideoCore.EGL.Types where

import Foreign
import Foreign.C

type NativeDisplayType = Ptr ()

-- egl.h --
-- EGL Types --
type Boolean = CUInt
type Enum    = CUInt
type Config  = Ptr ()
type Context = Ptr ()
type Display = Ptr ()
type Surface = Ptr ()
type ClientBuffer = Ptr ()


-- EGL Versioning --
version1_0 = CUInt 1
version1_1 = CUInt 1
version1_2 = CUInt 1
version1_3 = CUInt 1
version1_4 = CUInt 1

-- EGL Aliases --
false = Boolean 0
true  = Boolean 1

-- Out-of-band handle values --
defaultDisplay :: NativeDisplayType 
defaultDisplay = intPtrToPtr 0
