{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core where

import Foreign
import Foreign.C

foreign import ccall unsafe "bcm_host_init"
  bcm_host_init :: IO ()

foreign import ccall unsafe "bcm_host_deinit"
  bcm_host_deinit :: IO ()

foreign import ccall unsafe "graphics_get_display_size"
  graphics_get_display_size :: CUShort -> Ptr CUInt -> Ptr CUInt -> IO CInt
