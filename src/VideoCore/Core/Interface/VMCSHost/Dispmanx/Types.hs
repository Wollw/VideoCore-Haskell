{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core.Interface.VMCSHost.Dispmanx.Types where

import Foreign.C

version = CUInt 1

-- Opaque Handles --
type DisplayHandle  = CUInt
type UpdateHandle   = CUInt
type ElementHandle  = CUInt
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
type Status = CInt
success = CInt 0
invalid = CInt (-1)

type Transform = CUInt
noRotate  = CUInt 0
rotate90  = CUInt 1
rotate180 = CUInt 2
rotate270 = CUInt 3
flipHriz  = CUInt 65536
flipVert  = CUInt 131072

type FlagsAlpha = CUInt
flagsAlphaFromSource      = CUInt 0
flagsAlphaFixedAllPixels  = CUInt 1
flagsAlphaFixedNonZero    = CUInt 2
flagsAlphaFixedExceed0X07 = CUInt 3
flagsAlphaPreMult         = CUInt 65536
flagsAlphaMix             = CUInt 131072
