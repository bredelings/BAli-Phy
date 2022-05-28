module Data.Matrix where

import Foreign.Vector

data Matrix a

fromLists xss = fromVectors $ list_to_vector $ map list_to_vector xss

scaleMatrix :: (Num a) => a -> Matrix a -> Matrix a
foreign import bpcall "SModel:scaleMatrix" scaleMatrix :: () -> () -> ()
