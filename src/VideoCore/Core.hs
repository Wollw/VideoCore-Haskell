{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core where

import Foreign
--import Foreign.C

foreign import ccall unsafe "bcm_host_init" bcm_host_init :: IO ()
