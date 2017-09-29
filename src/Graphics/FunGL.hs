module Graphics.FunGL
  ( someFunc

  -- | VAO
  , ArrayObject
  , newArrayObject
  , bindArrayObject
  , deleteArrayObject

    -- | VBO
  , BufferObject
  , BufferType
  , makeBuffer
  , bindBuffer
  , bindBufferToAttribute
  
  ) where

import Graphics.FunGL.ArrayObject
import Graphics.FunGL.BufferObject
import Graphics.FunGL.ShaderProgram
