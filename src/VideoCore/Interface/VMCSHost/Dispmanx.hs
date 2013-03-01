module VideoCore.Interface.VMCSHost.Dispmanx where

import Foreign
import Foreign.C

import VideoCore.Core.Interface.VMCSHost.Dispmanx.Types
import VideoCore.Core.Interface.VCTypes.ImageTypes
import qualified VideoCore.Core.Interface.VMCSHost.Dispmanx as DMX

elementAdd :: UpdateHandle -> DisplayHandle -> CInt -> VCRect -> ResourceHandle -> VCRect -> Protection -> Ptr Alpha -> Ptr Clamp -> Transform -> IO ElementHandle
elementAdd uh dh l dr rh sr p a c t =
    alloca $ \destRectP ->
    alloca $ \srcRectP ->
        DMX.elementAdd uh dh l destRectP rh srcRectP p a c t
