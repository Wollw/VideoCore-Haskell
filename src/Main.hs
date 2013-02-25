module Main where

import Prelude hiding (fail)
import Control.Monad (when)
import System.Exit

import qualified System.Hardware.GPIO.Pin as P
import qualified VideoCore as VC
import qualified VideoCore.EGL as EGL
--import qualified VideoCore.EGL.Macros as EGLM
import Foreign

fail n msg = putStrLn msg >> (exitWith (ExitFailure n))

main :: IO ()
main = do
    VC.bcm_host_init
    eglSetup

eglSetup :: IO ()
eglSetup = do
    -- Initialize the EGL display --
    display <- EGL.getDisplay EGL.defaultDisplay
    when (display == EGL.noDisplay) $ fail 1 "No display found."
    result <- EGL.initialize display nullPtr nullPtr
    when (result == EGL.false) $ fail 1 "Could not initialize display."

    (result, count) <- getConfigCount display
    when (result == EGL.false) $ fail 1 "Could not get config count."

    print count

    return ()
  where

getConfigCount :: EGL.Display -> IO (EGL.Boolean, EGL.EGLint)
getConfigCount display = do
    alloca $ \xp -> do
        r <- EGL.getConfigs display nullPtr 0 xp
        c <- peek xp
        return (r, c)
    


--main = do
--    p <- P.init 3 Out
--    v1 <- P.read p
--    print v1
--    P.close p
