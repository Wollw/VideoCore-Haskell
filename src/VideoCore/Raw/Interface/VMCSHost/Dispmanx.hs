{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module VideoCore.Raw.Interface.VMCSHost.Dispmanx where

#include "HsVideoCoreRaw.h"

import Foreign
import Foreign.C

import VideoCore.Raw.Interface.VMCSHost.Dispmanx.Types
import VideoCore.Raw.Interface.VCTypes.ImageTypes

FFI_FUNC(vc_dispmanx_display_open,          Device -> IO DisplayHandle)
FFI_FUNC(vc_dispmanx_update_start,          Priority -> IO UpdateHandle)
FFI_FUNC(vc_dispmanx_element_add,           UpdateHandle -> DisplayHandle -> CInt -> Ptr VCRect -> ResourceHandle -> Ptr VCRect -> Protection -> Ptr Alpha -> Ptr Clamp -> Transform -> IO ElementHandle)
FFI_FUNC(vc_dispmanx_update_submit_sync,    UpdateHandle -> IO CInt)
