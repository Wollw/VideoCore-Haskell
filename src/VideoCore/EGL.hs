module VideoCore.EGL where

import Foreign
import qualified VideoCore.Raw.EGL as EGLC
import qualified VideoCore.Raw.EGL.Platform as EGLCP
import Data.Array.Storable

type ClientVersion = EGLC.EGLint

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

getConfigAttrib :: EGLC.Display -> EGLC.Config -> EGLC.EGLint -> IO (EGLC.Boolean, EGLC.EGLint)
getConfigAttrib disp cfg attr = do
    alloca $ \valP -> do
        r <- EGLC.getConfigAttrib disp cfg attr valP
        valR <- peek valP
        return (r, valR)

chooseConfig :: EGLC.Display -> [EGLC.EGLint] -> IO (EGLC.Boolean, EGLC.Config, EGLC.EGLint)
chooseConfig disp attrs = do
    withArray attrs $ \attrsP ->
        alloca $ \configP ->
            alloca $ \countP -> do
                r <- EGLC.chooseConfig disp attrsP configP 1 countP
                configR <- peek configP
                countR <- peek countP
                return (r, configR, countR)

createContext :: EGLC.Display -> EGLC.Config -> EGLC.Context -> ClientVersion -> IO EGLC.Context
createContext d cfg ctx version =
    withArray [ EGLC.contextClientVersion, version, EGLC.none ] $ \attrs ->
        EGLC.createContext d cfg ctx attrs


createWindowSurface :: EGLC.Display -> EGLC.Config -> EGLCP.DispmanxWindow -> [EGLC.EGLint] -> IO EGLC.Surface
createWindowSurface d cfg w attrs =
    with w $ \windowP -> do
        r <- case null attrs of
            True -> EGLC.createWindowSurface d cfg windowP nullPtr
            False -> withArray attrs $ \attrsP ->
                EGLC.createWindowSurface d cfg windowP attrsP
        return r
