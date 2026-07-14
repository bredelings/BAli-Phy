{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Foreign.CList (listToCList)
import Foreign.Vector (clist_to_vector, vector_size)
import System.IO (print)

main = print (vector_size (clist_to_vector (listToCList ["not a runtime value"])))
