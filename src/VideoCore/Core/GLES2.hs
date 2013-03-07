{-# LANGUAGE ForeignFunctionInterface #-}
module VideoCore.Core.GLES2 where

import Foreign
import Foreign.C

type GLuint = CUInt
type GLsizei = CInt
type GLint = CInt
type GLchar = CChar
type GLenum = CUInt
type GLfloat = CFloat

-- Shaders --
fragmentShader = 0x8B30 :: GLenum
vertexShader = 0x8B31 :: GLenum

foreign import ccall unsafe "glGetError"
  getError :: IO GLenum

foreign import ccall unsafe "glCreateShader"
  createShader :: GLenum -> IO GLuint

foreign import ccall unsafe "glShaderSource"
  shaderSource :: GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()

foreign import ccall unsafe "glGetAttribLocation"
  getAttribLocation :: GLuint -> Ptr GLchar -> IO GLint

foreign import ccall unsafe "glGetUniformLocation"
  getUniformLocation :: GLuint -> Ptr GLchar -> IO GLint

foreign import ccall unsafe "glCreateProgram"
  createProgram :: IO GLuint

foreign import ccall unsafe "glAttachShader"
  attachShader :: GLuint -> GLuint -> IO ()

foreign import ccall unsafe "glCompileShader"
  compileShader :: GLuint -> IO ()

foreign import ccall unsafe "glLinkProgram"
  linkProgram :: GLuint -> IO ()

foreign import ccall unsafe "glEnableVertexAttribArray"
  enableVertexAttribArray :: GLuint -> IO ()

foreign import ccall unsafe "glDisableVertexAttribArray"
  disableVertexAttribArray :: GLuint -> IO ()

foreign import ccall unsafe "glClearColor"
  clearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
