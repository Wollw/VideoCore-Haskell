module Main where

import qualified System.Hardware.GPIO.Pin as P
import qualified VideoCore as VC
import qualified VideoCore.EGL as EGL
import qualified VideoCore.EGL.Macros as EGLM
import Foreign

main :: IO ()
main = do
    VC.bcm_host_init
    display <- EGL.getDisplay EGLM.defaultDisplay
    print . ptrToIntPtr $ display
    return ()


--main = do
--    p <- P.init 3 Out
--    v1 <- P.read p
--    print v1
--    P.close p
