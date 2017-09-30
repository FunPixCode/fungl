module Graphics.FunGL.Linear
  (
  ) where

import Foreign.Ptr (Ptr, castPtr)

import Linear

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

castVecComponent :: Ptr (t a) -> Ptr a
castVecComponent = castPtr
