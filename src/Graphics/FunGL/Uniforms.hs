module Graphics.FunGL.Uniforms
  ( UniformLoc(..)
  , getUniformLocation -- get slot id for uniform
  , bindUniform1f -- bind Float
  , bindUniform1i -- bind Int
  , bindUniform44f -- bind matrix 4x4
  , bindUniform33f -- bind matrix 3x3
  , bindUniform22f -- bind matrix 2x1
  ) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal

import Linear
import Graphics.GL

import Graphics.FunGL.ShaderProgram (ShaderProgram(..))

newtype UniformLoc = UniformLoc { fromUniformLoc :: GLint }

-- | Bind uniform name to location specified by OpenGL
getUniformLocation :: ShaderProgram -> String -> IO UniformLoc
getUniformLocation prog name = do
  loc <- withCString name $ glGetUniformLocation $ fromShaderProgram prog
  if loc < 0
    then error $ "`" ++ name ++ "` can not be found!"
    else return $ UniformLoc (fromIntegral loc)

-- | Uniforms

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

castVecComponent :: Ptr (t a) -> Ptr a
castVecComponent = castPtr

-- | bind float single value uniform
bindUniform1f :: Float -> UniformLoc -> IO ()
bindUniform1f value loc = do
  glUniform1f (fromUniformLoc loc) value

-- | bind int single value uniform
bindUniform1i :: Int -> UniformLoc -> IO ()
bindUniform1i value loc = do
  glUniform1i (fromUniformLoc loc) (fromIntegral value)

-- | bind matrix 4x4
bindUniform44f :: M44 Float -> UniformLoc -> IO ()
bindUniform44f matrix loc = do
  with matrix $ glUniformMatrix4fv (fromUniformLoc loc) 1 (fromBool False) . castMatComponent

-- | bind matrix 3x3
bindUniform33f :: M33 Float -> UniformLoc -> IO ()
bindUniform33f matrix loc = do
  with matrix $ glUniformMatrix3fv (fromUniformLoc loc) 1 (fromBool False) . castMatComponent

-- | bind matrix 2x2
bindUniform22f :: M22 Float -> UniformLoc -> IO ()
bindUniform22f matrix loc = do
  with matrix $ glUniformMatrix2fv (fromUniformLoc loc) 1 (fromBool False) . castMatComponent

