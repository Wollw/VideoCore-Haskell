module VideoCore where

import qualified VideoCore.Raw as VCRaw
import qualified VideoCore.Core.EGL as EGL

import Foreign
import Foreign.C

bcmHostInit :: IO ()
bcmHostInit = VCRaw.bcm_host_init

bcmHostDeinit :: IO ()
bcmHostDeinit = VCRaw.bcm_host_deinit

graphicsGetDisplaySize :: CUShort -> IO (CInt, CUInt, CUInt)
graphicsGetDisplaySize dispNum = 
    alloca $ \wP ->
    alloca $ \hP -> do
        ret    <- VCRaw.graphics_get_display_size dispNum wP hP
        width  <- peek wP
        height <- peek hP
        return (ret, width, height)
