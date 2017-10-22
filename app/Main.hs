{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW

import Data.Vec
import Data.Maybe (fromJust, isNothing)
import Control.Monad (when, mapM_)
import System.Exit (exitFailure)

import Graphics.FunGL
import qualified Linear as L
import Data.IORef
import Graphics.GL -- temporary for uniform

import Foreign
import Foreign.C.String
import Foreign.C.Types

initialize :: IO GLFW.Window
initialize = do
  ok <- GLFW.init
  when (not ok) $ do
    _ <- fail "Failed to initilize GLFW"
    exitFailure

  mapM_ GLFW.windowHint [ GLFW.WindowHint'Samples 4 -- 4x antialiasing
                        , GLFW.WindowHint'ContextVersionMajor 3 -- OpenGL 3.3
                        , GLFW.WindowHint'ContextVersionMinor 3
                        , GLFW.WindowHint'OpenGLForwardCompat True
                        , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
                        ]
  win <- GLFW.createWindow 640 480 "FunGL" Nothing Nothing
  when (isNothing win) $ do
    _ <- fail "Failed to create OpenGL window"
    GLFW.terminate
    exitFailure

  let win' = fromJust win
  GLFW.makeContextCurrent win
  GLFW.swapInterval 1
  GLFW.setStickyKeysInputMode win' GLFW.StickyKeysInputMode'Enabled

  return win'

data Model = Model { program         :: !ShaderProgram -- shader program id
                   , arrayObject     :: !ArrayObject   -- VAO id
                   , vertexAttribLoc :: !AttrLoc       -- vertex atribute location
                   , vertexBuffer    :: !BufferObject  -- vertex VBO id
                   , colorAttribLoc  :: !AttrLoc       -- color attribute location
                   , colorBuffer     :: !BufferObject  -- color VBO id
                   , mvpUniformLoc   :: !UniformLoc    -- mvp uniform location
                   }


initializeGL :: IO Model
initializeGL = do
  clearColor (0.812, 0.957, 0.969, 1)

  program <- makeShaderProgram vertexShader2 fragmentShader2

  -- get slots for attributes
  vertexAttribLoc <- getAttribLocation program "vertexPosition_modelspace"
  colorAttribLoc <- getAttribLocation program "vertexColor"

  -- get slot for uniform
  mvpUniformLoc <- getUniformLocation program "MVP"

  -- prepare VBO ids
  vertexBuffer <- makeBuffer ArrayBuffer vertexBufferData
  colorBuffer <- makeBuffer ArrayBuffer colorBufferData

  -- prepare VAO
  arrayObject <- newArrayObject

  -- bind buffers to VAO slots
  bindBuffer vertexBuffer ArrayBuffer
  bindBufferToAttribute vertexBuffer vertexAttribLoc

  bindBuffer colorBuffer ArrayBuffer
  bindBufferToAttribute colorBuffer colorAttribLoc

  bindArrayObject (ArrayObject 0)

  return $ Model program arrayObject vertexAttribLoc vertexBuffer colorAttribLoc colorBuffer mvpUniformLoc

main :: IO ()
main = do
  win <- initialize
  model <- initializeGL

  t <- maybe 0 id <$> GLFW.getTime
  lastTime <- newIORef (t)
  nbFrames <- newIORef (0 :: Int)
  inputLoop win model (lastTime, nbFrames)

  -- free
  GLFW.terminate
  return ()

inputLoop :: GLFW.Window -> Model -> (IORef (Double), IORef (Int)) -> IO ()
inputLoop win model a@(lastTime, nbFrames) = do

  currentTime <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
  
  draw model currentTime

  -- frames
  modifyIORef nbFrames (+1)
  x <- readIORef lastTime
  when (currentTime - x >= 1.0) $ do
    nf <- readIORef nbFrames
    putStrLn $ (show (1000.0/(realToFrac nf))) ++ "ms per frame, FPS: " ++ show nf
    writeIORef nbFrames 0
    modifyIORef lastTime (+1.0)

  GLFW.swapBuffers win
  GLFW.pollEvents

  keyState <- GLFW.getKey win GLFW.Key'Escape
  closeWindow <- GLFW.windowShouldClose win

  when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
    inputLoop win model a

draw :: Model -> Double -> IO ()
draw model time = do
  clearColorBuffer
  clearDepthBuffer

  -- bind program to current context
  bindProgram (program model)

  bindUniform44f mvpMatrix' (mvpUniformLoc model)

  bindArrayObject (arrayObject model)

  -- glDrawArrays GL_TRIANGLES 0 3
  drawArrays 3

  bindArrayObject (ArrayObject 0)
  
  bindProgram (ShaderProgram 0)
  

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

vertexBufferData :: [Float]
vertexBufferData = [ -1, -1, 0
                   ,  1, -1, 0
                   ,  0,  1, 0
                   ]

colorBufferData :: [Float]
colorBufferData = [ 1, 0, 0
                  , 0, 1, 0
                  , 0, 0, 1
                  ]

fragmentShader2 :: String
fragmentShader2 = unlines
  [ "#version 330 core"
  , "in vec3 fragmentColor;"
  , "out vec3 finalColor;"
  , "void main()"
  , "{"
  , "finalColor= fragmentColor;"
  , "}"
  ]

vertexShader2 :: String
vertexShader2 = unlines
  [ "#version 330 core"
  -- Input vertex data, different for all executions of this shader.
  , "in vec3 vertexPosition_modelspace;"
  , "in vec3 vertexColor;"
  -- Values that stay constant for the whole mesh
  , "uniform mat4 MVP;"
  , "out vec3 fragmentColor;"
  , "void main()"
  , "{"
  , "fragmentColor = vertexColor;"
  , "vec4 v = vec4(vertexPosition_modelspace, 1);"
  , "gl_Position = MVP * v;"
  , "}"
  ]

-- Some higher-order math helper functions. Depending on what math
-- library you use, you'd use the functions that comes with that
-- library. The functions here are from the Data.Vec package.
vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z:. ()

mvpMatrix :: Mat44 Float
mvpMatrix = multmm (multmm projection view) model
  where
    projection = perspective 0.1 100 (pi/4) (4/3)
    view = lookAt (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
    model = identity

-- The closest relative to this function is Data.Vec's `rotationLookAt`. We just
-- mirror the code found in the GLM library (glm.g-truc.net). An additional
-- resource is Jeremiah van Oosten's "Understanding the View Matrix", found at
-- http://3dgep.com/?p=1700.
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt eye target up = x :. y :. z :. h :. ()
  where
    forward = normalize $ target - eye
    right = normalize $ cross forward up
    up' = cross right forward
    x = snoc right (-(dot right eye))
    y = snoc up' (-(dot up' eye))
    z = snoc (-forward) (dot forward eye)
    h = 0 :. 0 :. 0 :. 1 :. ()

cameraPos :: L.V3 Float
cameraPos = L.V3 0 0 3 -- eye

cameraTarget :: L.V3 Float
cameraTarget = L.V3 0 0 0 -- look at coord

up :: L.V3 Float
up = L.V3 0 1 0

mvpMatrix' :: L.M44 Float
mvpMatrix' = p L.!*! v L.!*! modelMatrix'
  where
    p = L.perspective (60 * 3.14/180) (3.14/4) 0.1 100
    v = L.lookAt cameraPos cameraTarget up

modelMatrix' :: L.M44 Float
modelMatrix' = L.V4
               (L.V4 1 0 0 0)
               (L.V4 0 1 0 0)
               (L.V4 0 0 0 0)
               (L.V4 0 0 0 1)
               
