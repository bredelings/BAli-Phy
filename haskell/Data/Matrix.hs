module Data.Matrix where

import Foreign.Vector

fromLists xss = fromVectors $ list_to_vector $ map list_to_vector xss

foreign import bpcall "SModel:scaleMatrix" scaleMatrix :: a -> Matrix a -> Matrix a
