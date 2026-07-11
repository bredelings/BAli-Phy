{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Classes
import Data.Eq
import qualified Data.Foldable as F
import Data.Maybe
import Data.Ord
import Data.Traversable (traverse)
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.fromList [1,2,3] :: V.Vector Int
    print values
    print (values == V.fromList [1,2,3])
    print (compare values (V.fromList [1,2,4]) == LT)
    print (F.foldl (+) 0 values)
    print (F.foldr (:) [] values)
    print (traverse (\value -> Just (value + 1)) values :: Maybe (V.Vector Int))
