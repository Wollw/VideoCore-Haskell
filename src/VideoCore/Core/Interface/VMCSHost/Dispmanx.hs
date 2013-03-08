{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core.Interface.VMCSHost.Dispmanx where

import Foreign
import Foreign.C

import VideoCore.Core.Interface.VMCSHost.Dispmanx.Types
import VideoCore.Core.Interface.VCTypes.ImageTypes

foreign import ccall unsafe "vc_dispmanx_display_open"
  displayOpen :: Device -> IO DisplayHandle

foreign import ccall unsafe "vc_dispmanx_update_start"
  updateStart :: Priority -> IO UpdateHandle

foreign import ccall unsafe "vc_dispmanx_element_add"
  elementAdd :: UpdateHandle -> DisplayHandle -> CInt -> Ptr VCRect -> ResourceHandle -> Ptr VCRect -> Protection -> Ptr Alpha -> Ptr Clamp -> Transform -> IO ElementHandle

foreign import ccall unsafe "vc_dispmanx_update_submit_sync"
  updateSubmitSync :: UpdateHandle -> IO CInt
