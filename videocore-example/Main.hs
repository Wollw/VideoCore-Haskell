module Main where

import Prelude hiding (fail)
import Control.Monad (when, forever)
import System.Exit
import Numeric (showHex)

import qualified System.Hardware.GPIO.Pin as P

import qualified VideoCore as VC
import qualified VideoCore.EGL as EGL
import qualified VideoCore.Raw.EGL as EGLC
import qualified VideoCore.Raw.EGL.Platform as EGLCP
import qualified VideoCore.Raw.Interface.VCTypes.ImageTypes as VCTI
import qualified VideoCore.Raw.Interface.VMCSHost.Dispmanx as DispmanxRaw
import qualified VideoCore.Raw.Interface.VMCSHost.Dispmanx.Types as DispmanxTypes
import qualified VideoCore.Interface.VMCSHost.Dispmanx as Dispmanx
import qualified VideoCore.Raw.GLES2 as GLCore
import qualified VideoCore.GLES2 as GL
--import qualified VideoCore.EGL.Macros as EGLM
import Foreign
import Foreign.C
import Data.Array.Storable

data EGLState = EGLState
    { eglDisplay :: EGLC.Display
    , eglSurface :: EGLC.Surface
    , eglContext :: EGLC.Context
    , eglWidth   :: CUInt
    , eglHeight  :: CUInt
    } deriving (Show)

fail n msg = putStrLn msg >> (exitWith (ExitFailure n))

checkGL :: IO ()
checkGL = do
    e <- GLCore.getError
    case e of
        0 -> return ()
        e -> fail 1 $ "GL state failed assert."

main :: IO ()
main = do
    VC.bcmHostInit
    egl <- eglSetup
    checkGL
    shader <- createShaderProgram
    checkGL

    vPosition <- fmap (fromIntegral) $ GL.getAttribLocation shader "vPosition"
    checkGL
    GLCore.enableVertexAttribArray (fromIntegral vPosition)
    checkGL
    uColor <- fmap (fromIntegral) $ GL.getUniformLocation shader "uColor"
    checkGL
    
    GLCore.clearColor 0.5 0.5 1.0 1.0
    checkGL

    -- Configure viewport --
    GLCore.viewport 0 0 (fromIntegral . eglWidth $ egl) (fromIntegral . eglHeight $ egl)
    checkGL

    -- Upload vertex data --
    vertexBuffer <- GL.genBuffer
    checkGL
    GLCore.bindBuffer GLCore.arrayBuffer vertexBuffer
    checkGL
    GL.bufferData GLCore.arrayBuffer vertexData GLCore.staticDraw
    checkGL
    GLCore.bindBuffer GLCore.arrayBuffer 0
    checkGL

    forever $ do
        GLCore.clear GLCore.colorBufferBit
        GLCore.useProgram shader
        GLCore.uniform4f uColor 1.0 0.0 0.0 1.0
        GLCore.bindBuffer GLCore.arrayBuffer vertexBuffer
        GLCore.vertexAttribPointer vPosition 3 GLCore.float GLCore.false 0 nullPtr
        GLCore.drawArrays GLCore.triangles 0 3
        GLCore.bindBuffer GLCore.arrayBuffer 0
        GLCore.flush
        EGLC.swapBuffers (eglDisplay egl) (eglSurface egl)
    
    VC.bcmHostDeinit
  where
    vertexData = [ -0.5, -0.5, -1.0
                 ,  0.0,  0.5, -1.0
                 ,  0.5, -0.5, -1.0
                 ]
                

eglSetup :: IO EGLState
eglSetup = do
    -- Initialize the EGL display --
    display <- EGLC.getDisplay EGLC.defaultDisplay
    when (display == EGLC.noDisplay) $ fail 1 "No display found."
    checkGL
    result <- EGLC.initialize display nullPtr nullPtr
    when (result == EGLC.false) $ fail 1 "Could not initialize display."
    checkGL

    -- Get the EGL configs --
    (result, _, count) <- EGL.getConfigs display 0
    when (result == EGLC.false) $ fail 1 "Could not get config count."
    checkGL
    (result, configs, count) <- EGL.getConfigs display count
    when (result == EGLC.false) $ fail 1 "Could not get configs."
    checkGL

    -- Configure the EGL attributes --
    let n = 0
    (result, redSize) <- EGL.getConfigAttrib display (plusPtr configs n) EGLC.redSize
    when (result == EGLC.false) $ fail 1 "Could not get attribute value."
    (result, greenSize) <- EGL.getConfigAttrib display (plusPtr configs n) EGLC.greenSize
    when (result == EGLC.false) $ fail 1 "Could not get attribute value."
    (result, blueSize) <- EGL.getConfigAttrib display (plusPtr configs n) EGLC.blueSize
    when (result == EGLC.false) $ fail 1 "Could not get attribute value."
    (result, alphaSize) <- EGL.getConfigAttrib display (plusPtr configs n) EGLC.alphaSize
    when (result == EGLC.false) $ fail 1 "Could not get attribute value."
    (result, surfaceType) <- EGL.getConfigAttrib display (plusPtr configs n) EGLC.surfaceType
    when (result == EGLC.false) $ fail 1 "Could not get attribute value."
    (result, config, _) <- EGL.chooseConfig display
        [ EGLC.redSize, redSize
        , EGLC.greenSize, greenSize
        , EGLC.blueSize, blueSize
        , EGLC.alphaSize, alphaSize
        , EGLC.surfaceType, surfaceType
        , EGLC.none ]
    when (result == EGLC.false) $ fail 1 "Could not get config."
    checkGL
    result <- EGLC.bindAPI EGLC.openGLESAPI
    checkGL
    when (result == EGLC.false) $ fail 1 "Could not bind API."

    -- Create the EGL rendering context --
    context <- EGL.createContext display config EGLC.noContext 2
    when (context == EGLC.noContext) $ fail 1 "Count not create context."
    checkGL

    -- Create the EGL window surface --
    (result, width, height) <- VC.graphicsGetDisplaySize 0 -- LCD is 0
    when (result < 0) $ fail 1 "Failed to get display size."

    let dstRect = VCTI.VCRect {
          VCTI.vcRectX = 0
        , VCTI.vcRectY = 0
        , VCTI.vcRectWidth  = fromIntegral width
        , VCTI.vcRectHeight = fromIntegral height
        }
    let srcRect = VCTI.VCRect {
          VCTI.vcRectX = 0
        , VCTI.vcRectY = 0
        , VCTI.vcRectWidth  = fromIntegral width `shift` 16
        , VCTI.vcRectHeight = fromIntegral height `shift` 16
        }
    dispmanxDisplay <- DispmanxRaw.vc_dispmanx_display_open 0 -- LCD is 0
    dispmanxUpdate <- DispmanxRaw.vc_dispmanx_update_start 0
    dispmanxElement <- Dispmanx.elementAdd
        dispmanxUpdate dispmanxDisplay
        0 dstRect 0 srcRect DispmanxTypes.protectionNone
        nullPtr nullPtr 0
    let nativeWindow = EGLCP.DispmanxWindow {
          EGLCP.dmxWinElem = dispmanxElement
        , EGLCP.dmxWinWidth = width
        , EGLCP.dmxWinHeight = height
        }
    DispmanxRaw.vc_dispmanx_update_submit_sync dispmanxUpdate
    checkGL

    surface <- EGL.createWindowSurface display config nativeWindow []
    when (surface == EGLC.noSurface) $ fail 1 "Failed to create EGL surface."

    -- Connect the context to the surface --
    result <- EGLC.makeCurrent display surface surface context
    when (result == EGLC.false) $ fail 1 "Failed to connect context to surface."
    
    return (EGLState display surface context width height)

createShaderProgram :: IO GLCore.GLuint
createShaderProgram = do
    -- Create shaders --
    vertShader <- GLCore.createShader GLCore.vertexShader
    fragShader <- GLCore.createShader GLCore.fragmentShader
    readFile "shader.vert" >>= GL.shaderSource vertShader
    readFile "shader.frag" >>= GL.shaderSource fragShader
    mapM GLCore.compileShader [vertShader, fragShader]

    -- Create program --
    program <- GLCore.createProgram
    mapM (GLCore.attachShader program) [vertShader, fragShader]
    GLCore.linkProgram program

    return program
