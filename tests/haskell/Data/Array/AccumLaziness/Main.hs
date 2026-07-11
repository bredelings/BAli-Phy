{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

data Box = Box Int

-- Force a combining result only to WHNF without independently forcing its old
-- value, new value, lazy field, or an unrelated initial slot.
main = do
    let values = A.accumArray (\_ new -> Box new)
            (error "old value was forced") (0,1)
            [(0,error "new value was forced")] :: A.Array Int Box
    case values A.! 0 of
        Box _ -> print (1 :: Int)
    print (A.numElements values)
