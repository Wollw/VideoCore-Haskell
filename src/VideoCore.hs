module VideoCore where

import qualified VideoCore.Core as VC
import qualified VideoCore.Core.EGL as EGL

initialize :: IO ()
initialize = VC.bcm_host_init
