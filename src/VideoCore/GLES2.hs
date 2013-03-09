module VideoCore.GLES2 where

import qualified VideoCore.Core.GLES2 as GLCore

import Foreign
import Foreign.C

shaderSource :: GLCore.GLuint -> String -> IO ()
shaderSource shader source = do
    src <- newCString source
    with src $ \srcP ->
        GLCore.shaderSource shader 1 srcP nullPtr

getAttribLocation :: GLCore.GLuint -> String -> IO GLCore.GLint
getAttribLocation program attrib = do
    attribCStr <- newCString attrib
    GLCore.getAttribLocation program attribCStr

getUniformLocation :: GLCore.GLuint -> String -> IO GLCore.GLint
getUniformLocation program uniform = do
    uniformCStr <- newCString uniform
    GLCore.getUniformLocation program uniformCStr

genBuffer :: IO GLCore.GLuint
genBuffer = alloca $ \bufferP -> do
    GLCore.genBuffers 1 bufferP
    peek bufferP

bufferData :: GLCore.GLenum -> [GLCore.GLfloat] -> GLCore.GLenum -> IO ()
bufferData target bData usage = withArray bData $ \dataP ->
    GLCore.bufferData target (fromIntegral (length bData) * 4) (castPtr dataP) usage
