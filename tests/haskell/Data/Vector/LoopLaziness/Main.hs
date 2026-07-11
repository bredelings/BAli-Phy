{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import Compiler.Classes
import Data.Eq
import qualified Data.Foldable as F
import Data.Ord
import qualified Data.Vector as V
import System.IO (print)

-- Exercise early termination and accumulator laziness in every indexed loop
-- whose cached length must not make element evaluation stricter.
main = do
    print (F.foldr (\value _ -> value) 0
        (V.fromList [11,error "foldr tail was forced"] :: V.Vector Int))
    print (F.foldl (\_ value -> value) (error "foldl initial was forced")
        (V.fromList [error "foldl intermediate was forced",22] :: V.Vector Int))
    print (F.foldl1 (\_ value -> value)
        (V.fromList [error "foldl1 first value was forced",33] :: V.Vector Int))
    print (F.foldr1 (\value _ -> value)
        (V.fromList [44,error "foldr1 last value was forced"] :: V.Vector Int))
    print (V.elemIndex 55
        (V.fromList [55,error "elemIndex continued after a match"] :: V.Vector Int))
    print
        ((V.fromList [error "equality forced an element"] :: V.Vector Int) == V.empty,
         compare
            (V.fromList [1,error "left ordering tail was forced"] :: V.Vector Int)
            (V.fromList [2,error "right ordering tail was forced"] :: V.Vector Int) == LT,
         compare
            (V.fromList [1] :: V.Vector Int)
            (V.fromList [1,error "longer ordering tail was forced"] :: V.Vector Int) == LT)
