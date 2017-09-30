module Graphics.FunGL.Render
  ( clearColor
  , clearColorBuffer
  , clearDepthBuffer
  ) where

import Graphics.GL

-- | set clear color
clearColor :: (Float, Float, Float, Float) -> IO ()
clearColor (r, g, b, a) = glClearColor r g b a

-- | clear color
clearColorBuffer :: IO ()
clearColorBuffer = glClear GL_COLOR_BUFFER_BIT

-- | clear depth buffer
clearDepthBuffer :: IO ()
clearDepthBuffer = glClear GL_DEPTH_BUFFER_BIT
