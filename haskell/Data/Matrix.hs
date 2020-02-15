module Data.Matrix where

import Foreign.Vector

fromLists xss = fromVectors $ list_to_vector $ map list_to_vector xss

builtin scaleMatrix 2 "scaleMatrix" "SModel"
