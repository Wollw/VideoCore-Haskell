{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core.Interface.VMCSHost.Dispmanx.Types where

import Foreign
import Foreign.C

#include <interface/vmcs_host/vc_dispmanx_types.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


--Added in bindings, not in original headers
type Priority = CInt
type Device = CUInt
-- End added for bindings

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

-- DUMMY TYPE
data Alpha = Alpha

data ClampKeys = ClampKeys CUChar CUChar CUChar CUChar CUChar CUChar deriving (Show)

instance Storable ClampKeys where
    alignment _ = #{alignment DISPMANX_CLAMP_KEYS_T}
    sizeOf _ = #{size DISPMANX_CLAMP_KEYS_T}
    peek ptr = do
        rU <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.red_upper} ptr
        rL <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.red_lower} ptr
        gU <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.green_upper} ptr
        gL <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.green_lower} ptr
        bU <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.blue_upper} ptr
        bL <- #{peek DISPMANX_CLAMP_KEYS_T, rgb.blue_lower} ptr
        return (ClampKeys rU rL gU gL bU bL)
    poke ptr (ClampKeys rU rL gU gL bU bL) = do
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.red_upper} ptr rU
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.red_lower} ptr rL
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.green_upper} ptr gU
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.green_lower} ptr gL
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.blue_upper} ptr bU
        #{poke DISPMANX_CLAMP_KEYS_T, rgb.blue_lower} ptr bL

data Clamp = Clamp
    { clampMode :: CUInt
    , clampKeyMask :: CUInt
    , clampKeyValue  :: ClampKeys
    , clampReplaceValue :: CUInt
    } deriving (Show)

instance Storable Clamp where
    alignment _ = #{alignment DISPMANX_CLAMP_T}
    sizeOf _ = #{size DISPMANX_CLAMP_T}
    peek ptr = do
        m <- #{peek DISPMANX_CLAMP_T, mode} ptr
        km <- #{peek DISPMANX_CLAMP_T, key_mask} ptr
        kv <- #{peek DISPMANX_CLAMP_T, key_value} ptr
        rv <- #{peek DISPMANX_CLAMP_T, replace_value} ptr
        return (Clamp m km kv rv)
    poke ptr (Clamp m km kv rv) = do
        #{poke DISPMANX_CLAMP_T, mode} ptr m
        #{poke DISPMANX_CLAMP_T, key_mask} ptr km
        #{poke DISPMANX_CLAMP_T, key_value} ptr kv
        #{poke DISPMANX_CLAMP_T, replace_value} ptr rv
