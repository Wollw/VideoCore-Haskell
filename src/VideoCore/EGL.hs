module VideoCore.EGL where

import Foreign
import qualified VideoCore.Core.EGL as EGLC

getConfigs :: EGLC.Display -> EGLC.EGLint -> IO (EGLC.Boolean, EGLC.Config, EGLC.EGLint)
getConfigs display count = do
    case count of
        0 -> do
            (r, c) <- getConfigCount display
            return (r, nullPtr, c)
        _ -> do
            allocaBytes ((fromIntegral count) * 4) $ \configsP ->
                alloca $ \countP -> do
                    r <- EGLC.getConfigs display configsP count countP
                    configsR <- peek configsP
                    countR <- peek countP
                    return (r, configsR, countR)
  where
    getConfigCount display = do
        alloca $ \countP -> do
            r <- EGLC.getConfigs display nullPtr 0 countP
            countR <- peek countP
            return (r, countR)
