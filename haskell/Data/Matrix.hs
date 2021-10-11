module Data.Matrix where

import Foreign.Vector

data Matrix a

fromLists xss = fromVectors $ list_to_vector $ map list_to_vector xss

scaleMatrix :: (Num a) => a -> Matrix a -> Matrix a
builtin scaleMatrix 2 "scaleMatrix" "SModel"
