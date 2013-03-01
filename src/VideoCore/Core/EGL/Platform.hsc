module VideoCore.Core.EGL.Platform where

import Foreign
import Foreign.C

import qualified VideoCore.Core.Interface.VMCSHost.Dispmanx.Types as DMXTypes

#include <EGL/eglplatform.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data DispmanxWindow = DispmanxWindow
    { dmxWinElem   :: DMXTypes.ElementHandle
    , dmxWinWidth  :: CUInt
    , dmxWinHeight :: CUInt
    } deriving (Show)

instance Storable DispmanxWindow where
    alignment _ = #{alignment EGL_DISPMANX_WINDOW_T}
    sizeOf _ = #{size EGL_DISPMANX_WINDOW_T}
    peek ptr = do
        e <- #{peek EGL_DISPMANX_WINDOW_T, element} ptr
        w <- #{peek EGL_DISPMANX_WINDOW_T, width} ptr
        h <- #{peek EGL_DISPMANX_WINDOW_T, height} ptr
        return (DispmanxWindow e w h)
    poke ptr (DispmanxWindow e w h) = do
        #{poke EGL_DISPMANX_WINDOW_T, element} ptr e
        #{poke EGL_DISPMANX_WINDOW_T, width} ptr w
        #{poke EGL_DISPMANX_WINDOW_T, height} ptr h
