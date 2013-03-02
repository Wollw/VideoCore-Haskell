{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core.GLES2 where

import Foreign
import Foreign.C

type GLuint = CUInt
type GLsizei = CInt
type GLint = CInt
type GLchar = CChar
type GLenum = CUInt

-- Shaders --
fragmentShader = 0x8B30 :: GLenum
vertexShader = 0x8B31 :: GLenum

foreign import ccall unsafe "glCreateShader"
  createShader :: GLenum -> IO GLuint

foreign import ccall unsafe "glShaderSource"
  shaderSource :: GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
