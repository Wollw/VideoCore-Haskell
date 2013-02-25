{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.EGL where

import Foreign
import Foreign.C

type NativeDisplayType = Ptr ()

type EGLint = CInt

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
false = CUInt 0 :: Boolean
true  = CUInt 1 :: Boolean

-- Out-of-band handle values --
defaultDisplay = intPtrToPtr 0 :: NativeDisplayType
noContext      = intPtrToPtr 0 :: Context
noDisplay      = intPtrToPtr 0 :: Display
noSurface      = intPtrToPtr 0 :: Surface

-- Out-of-band attribute value --
-- dontCare = -1 :: EGLint
dontCare = -1 :: CInt

-- Errors / getError return values --
success = 0x3000 :: CUInt
notInitialized = 0x3001 :: CUInt
badAccess = 0x3002 :: CUInt
badAlloc = 0x3003 :: CUInt
badAttribute = 0x3004 :: CUInt
badConfig = 0x3005 :: CUInt
badContext = 0x3006 :: CUInt
badCurrentSurface = 0x3007 :: CUInt
badDisplay = 0x3008 :: CUInt
badMatch = 0x3009 :: CUInt
badNativePixmap = 0x300A :: CUInt
badNativeWindow = 0x300B :: CUInt
badParameter = 0x300C :: CUInt
badSurface = 0x300D :: CUInt
contextLost = 0x300E :: CUInt -- EGL 1.1 - IMG_power_management

-- Config attributes -- TODO Mostly Unimplemented
bufferSize = 0x3020 :: CUInt
alphaSize = 0x3021 :: CUInt
blueSize = 0x3022 :: CUInt
greenSize = 0x3023 :: CUInt
redSize =  0x3024 :: CUInt
--
surfaceType = 0x3033
--
none = 0x3038 :: CUInt

-- Config attribute values -- TODO

-- More config attribute values, for EGL_TEXTURE_FORMAT -- TODO

-- Config attribute mask bits -- TODO Mostly Unimplemented
--
windowBit = 0x0004 :: CUInt

-- QueryString targets -- TODO

-- QuerySurface / Surface Attrib / CreatePbufferSurface targets -- TODO

-- EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets -- TODO

-- OpenVG color spaces -- TODO

-- OpenVG alpha formats -- TODO

-- Constant scale factor by which fractional display resolutions &
-- aspect ratio are scaled when queried as integer values.
displayScaling = 10000 :: CUInt

-- Unknown display resolution/aspect ratio --
--unknown = -1 :: EGLint
unknown = -1 :: CInt

-- Back buffer swap behaviors -- TODO

-- CreatePbufferFromClientBuffer buffer types -- TODO

-- QueryContext targets -- TODO

-- QueryContext attributes --
contextClientVersion = 0x3098 :: CUInt

----

foreign import ccall unsafe "eglGetDisplay"
  getDisplay :: NativeDisplayType -> IO Display

foreign import ccall unsafe "eglInitialize"
  initialize :: Display -> Ptr EGLint -> Ptr EGLint -> IO Boolean

foreign import ccall unsafe "eglGetConfigs"
  getConfigs :: Display -> Ptr Config -> EGLint -> Ptr EGLint -> IO Boolean
