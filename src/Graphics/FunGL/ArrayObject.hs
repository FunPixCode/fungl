module Graphics.FunGL.ArrayObject
  ( someFunc

  -- | VAO
  , ArrayObject
  , newArrayObject
  , bindArrayObject
  , deleteArrayObject
  
  ) where

import Foreign.Storable (peek)
import Foreign.Marshal (alloca, with)

import Graphics.GL

newtype ArrayObject = ArrayObject { fromArrayObject :: GLuint } deriving Show

-- | create new VAO
newArrayObject :: IO ArrayObject
newArrayObject = do
  identifier <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
  glBindVertexArray identifier
  return (ArrayObject identifier)

-- | bind VAO to current context
bindArrayObject :: ArrayObject -> IO ()
bindArrayObject = glBindVertexArray . fromArrayObject

-- | delete VAO
deleteArrayObject :: ArrayObject -> IO ()
deleteArrayObject (ArrayObject identifier) = do
  with identifier $ glDeleteVertexArrays 1

someFunc :: IO ()
someFunc = putStrLn "Yoo hoo, some func"
