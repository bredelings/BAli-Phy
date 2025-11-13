{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple.Extra (module Data.Tuple,
                         module Data.Tuple.Extra)
    where

import Data.Tuple
    
first f (x,y) = (f x, y)

second f (x, y) = (x, f y)

both f (x, y) = (f x, f y)                  

(f *** g) (x, y) = (f x, g y)                

(f &&& g) x = (f x, g x)                   

infixr 3 ***, &&&
