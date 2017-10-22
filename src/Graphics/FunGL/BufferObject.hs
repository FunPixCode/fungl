module Graphics.FunGL.BufferObject
  (
    BufferObject
  , BufferType(..)
  , makeBuffer
  , bindBuffer
  , deleteBuffer
  , bindBufferToAttribute
  ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal (alloca, with, withArray, fromBool)
import Foreign.Storable (Storable, peek, sizeOf)

import Graphics.FunGL.ShaderProgram
import Graphics.FunGL.Attributes

import Graphics.GL

data BufferType = ArrayBuffer
                | ElementBuffer deriving Show

newtype BufferObject = BufferObject { fromBufferObject :: GLuint }

-- | create vertex buffer object of given type (VBO)
makeBuffer :: Storable a => BufferType -> [a] -> IO BufferObject
makeBuffer bufferType elements = do
  case bufferType of
    ArrayBuffer   -> makeBuffer' GL_ARRAY_BUFFER elements
    ElementBuffer -> makeBuffer' GL_ELEMENT_ARRAY_BUFFER elements

makeBuffer' :: Storable a => GLenum -> [a] -> IO BufferObject
makeBuffer' bufferType elements = do
  bufferId <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer bufferType bufferId

  withArray elements $ \ptr -> do
    let size = fromIntegral ((length elements) * sizeOf (head elements))
    glBufferData bufferType size ptr GL_STATIC_DRAW

  glBindBuffer bufferType 0

  return (BufferObject bufferId)

-- | Bind buffer to current context (VBO)
bindBuffer :: BufferObject -> BufferType  -> IO ()
bindBuffer bufferObject bufferType = do
  case bufferType of
    ArrayBuffer   -> glBindBuffer GL_ARRAY_BUFFER (fromBufferObject bufferObject)
    ElementBuffer -> glBindBuffer GL_ELEMENT_ARRAY_BUFFER (fromBufferObject bufferObject)

-- | delete buffer object (VBO)
deleteBuffer :: BufferObject -> IO ()
deleteBuffer bufferObject = do
  with (fromBufferObject bufferObject) $ glDeleteBuffers 1

-- | bind buffer to attribute
bindBufferToAttribute :: BufferObject -> AttrLoc -> IO ()
bindBufferToAttribute bufferObject attribLoc = do
  glEnableVertexAttribArray $ fromAttrLoc attribLoc
  glBindBuffer GL_ARRAY_BUFFER (fromBufferObject bufferObject)
  glVertexAttribPointer
    (fromAttrLoc attribLoc) -- attribute location in the shader
    3 -- 3 components per vertex (x,y,z)
    GL_FLOAT -- coord type
    (fromBool False) -- normalize?
    0
    nullPtr
