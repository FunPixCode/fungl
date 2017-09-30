module Graphics.FunGL
  ( someFunc

    -- | VAO
  , ArrayObject(..)
  , newArrayObject
  , bindArrayObject
  , deleteArrayObject

    -- | VBO
  , BufferObject(..)
  , BufferType(..)
  , makeBuffer
  , bindBuffer
  , deleteBuffer
  , bindBufferToAttribute

    -- | shader program
  , AttrLoc(..)
  , UniformLoc(..)
  , ShaderProgram(..)
  , makeShaderProgram
  , bindProgram
  , deleteProgram
  , bindAttribLocation
  , getAttribLocation
  , getUniformLocation

    -- | rendering helpers
  , clearColor
  , clearColorBuffer
  , clearDepthBuffer
  
  ) where

import Graphics.FunGL.ArrayObject
import Graphics.FunGL.BufferObject
import Graphics.FunGL.ShaderProgram
import Graphics.FunGL.Render
