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
type GLsizeiptr = CLong
type GLboolean = CUChar
type GLvoid = ()

-- DataType --
float = 0x1406 :: GLenum

-- Boolean --
true  = 1 :: GLboolean
false = 0 :: GLboolean

-- BeginMode --
points        = 0x0 :: GLenum
lines         = 0x1 :: GLenum
lineLoop      = 0x2 :: GLenum
lineStrip     = 0x3 :: GLenum
triangles     = 0x4 :: GLenum
triangleStrip = 0x5 :: GLenum
triangleFan   = 0x6 :: GLenum

-- ClearBufferMask --
colorBufferBit = 0x00004000 :: GLenum

-- Shaders --
fragmentShader = 0x8B30 :: GLenum
vertexShader = 0x8B31 :: GLenum

-- Buffer Objects --
arrayBuffer = 0x8892 :: GLenum

staticDraw = 0x88E4 :: GLenum

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

foreign import ccall unsafe "glGenBuffers"
  genBuffers :: GLsizei -> Ptr GLuint -> IO ()

foreign import ccall unsafe "glBindBuffer"
  bindBuffer :: GLenum -> GLuint -> IO ()

foreign import ccall unsafe "glBufferData"
  bufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ()

foreign import ccall unsafe "glViewport"
  viewport :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()

foreign import ccall unsafe "glClear"
  clear :: GLenum -> IO ()

foreign import ccall unsafe "glUseProgram"
  useProgram :: GLuint -> IO ()

foreign import ccall unsafe "glUniform4f"
  uniform4f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import ccall unsafe "glVertexAttribPointer"
  vertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr GLvoid -> IO ()

foreign import ccall unsafe "glDrawArrays"
  drawArrays :: GLenum -> GLint -> GLsizei -> IO ()

foreign import ccall unsafe "glFlush"
  flush :: IO ()
