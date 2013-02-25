{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Interface.VMCSHost.Dispmanx.Types where

version = CUInt 1

-- Opaque Handles --
type DisplayHandle  = CUInt
type UpdateHandle   = CUInt
type ElementHandle  = CUint
type ResourceHandle = CUInt

type Protection = CUInt

noHandle       = CUInt 0
protectionMax  = CUInt 0x0f
protectionNone = CUInt 0
protectionHDCP = CUInt 11

-- Default display IDs --
idMainLCD = CUInt 0
idAuxLCD  = CUInt 1
idHDMI    = CUInt 2
idSDTV    = CUInt 3

-- Return codes. Nonzero ones indicate failure. --
type Status = CUInt
success = Status 0
invalid = Status -1

type Transform = CUInt
noRotate  = Transform 0
rotate90  = Transform 1
rotate180 = Transform 2
rotate270 = Transform 3
flipHriz  = Transform 65536
flipVert  = Transform 131072

type FlagsAlpha = CUint
flagsAlphaFromSource      = FlagsAlpha 0
flagsAlphaFixedAllPixels  = FlagsAlpha 1
flagsAlphaFixedNonZero    = FlagsAlpha 2
flagsAlphaFixedExceed0X07 = FlagsAlpha 3
flagsAlphaPreMult         = FlagsAlpha 65536
flagsAlphaMix             = FlagsAlpha 131072
