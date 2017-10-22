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
  , ShaderProgram(..)
  , makeShaderProgram
  , bindProgram
  , deleteProgram

  -- | uniforms
  , UniformLoc(..)
  , getUniformLocation
  , bindUniform1f
  , bindUniform1i
  , bindUniform44f
  , bindUniform33f
  , bindUniform22f

  -- | attributes
  , AttrLoc(..)
  , bindAttribLocation
  , getAttribLocation

  -- | rendering helpers
  , clearColor
  , clearColorBuffer
  , clearDepthBuffer
  , drawArrays
  , drawIndex

  ) where

import Graphics.FunGL.ArrayObject
import Graphics.FunGL.BufferObject
import Graphics.FunGL.ShaderProgram
import Graphics.FunGL.Render
import Graphics.FunGL.Uniforms
import Graphics.FunGL.Attributes
