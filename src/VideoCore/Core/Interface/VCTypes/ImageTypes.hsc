module VideoCore.Core.Interface.VCTypes.ImageTypes where

import Foreign
import Foreign.C

#include <interface/vctypes/vc_image_types.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data VCRect = VCRect
    { vcRectX :: CInt
    , vcRectY :: CInt
    , vcRectWidth  :: CInt
    , vcRectHeight :: CInt
    } deriving (Show)

instance Storable VCRect where
    alignment _ = #{alignment VC_RECT_T}
    sizeOf _ = #{size VC_RECT_T}
    peek ptr = do
        x <- #{peek VC_RECT_T, x} ptr
        y <- #{peek VC_RECT_T, y} ptr
        w <- #{peek VC_RECT_T, width} ptr
        h <- #{peek VC_RECT_T, height} ptr
        return (VCRect x y w h)
    poke ptr (VCRect x y w h) = do
        #{poke VC_RECT_T, x} ptr x
        #{poke VC_RECT_T, y} ptr y
        #{poke VC_RECT_T, width} ptr w
        #{poke VC_RECT_T, height} ptr h
