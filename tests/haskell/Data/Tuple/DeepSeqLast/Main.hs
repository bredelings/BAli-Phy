{-# LANGUAGE NoImplicitPrelude #-}
import Compiler.Base (seq)
import Compiler.Num
import Compiler.Error (error)
import Control.DeepSeq (rnf)
import System.IO (print)

-- Successful scalar forcing cannot detect a skipped tuple component. This checks
-- the final field of the largest instance; uniform derivation would make it redundant.
main = rnf ((),(),(),(),(),(),(),(),error "last tuple field" :: Int) `seq` print (0 :: Int)
