module Type.Trout.Headers where

import Data.Map (Map)

class Headers a where
  headers :: a -> Map String String
