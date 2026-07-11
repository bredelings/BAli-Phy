{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import Data.Bool
import Data.Ix
import Data.Ord
import System.IO (print)

data BadIndex = BadIndex deriving Eq deriving Ord

instance Ix BadIndex where
    range _ = [BadIndex]
    index _ _ = 1
    inRange _ _ = True
    rangeSize _ = 1

-- Independently validate the Ix offset against the stored backing count.
main = do
    let values = A.listArray (BadIndex,BadIndex) [10]
            :: A.Array BadIndex Int
    print (values A.! BadIndex)
