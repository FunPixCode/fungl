module Graphics.FunGL.Attributes
  ( AttrLoc(..)
  , bindAttribLocation
  , getAttribLocation
  ) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable

import Graphics.GL

import Graphics.FunGL.ShaderProgram

newtype AttrLoc = AttrLoc { fromAttrLoc :: GLuint }

-- | Bind attribute name to specified location
bindAttribLocation :: ShaderProgram -> AttrLoc -> String -> IO ()
bindAttribLocation prog loc name = do
  withCString name $ glBindAttribLocation (fromShaderProgram prog) (fromAttrLoc loc)

-- | Bind attribute name to location specified by OpenGL
getAttribLocation :: ShaderProgram -> String -> IO AttrLoc
getAttribLocation prog name = do
  loc <- withCString name $ glGetAttribLocation $ fromShaderProgram prog
  if loc < 0
    then error $ "`" ++ name ++ "` can not be found!"
    else return $ AttrLoc (fromIntegral loc)

