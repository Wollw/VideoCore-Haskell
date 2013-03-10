{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module VideoCore.Raw where

#include "HsVideoCoreRaw.h"

import Foreign
import Foreign.C

FFI_FUNC(bcm_host_init, IO ())
FFI_FUNC(bcm_host_deinit, IO())
FFI_FUNC(graphics_get_display_size, CUShort -> Ptr CUInt -> Ptr CUInt -> IO CInt)
