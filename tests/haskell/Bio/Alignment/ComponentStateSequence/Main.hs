{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (ComponentStateSequence(ComponentStateSequence),
                      componentStates, encodeComponentStateSequence)
import Compiler.Num
import qualified Data.JSON as JSON
import qualified Data.Text.IO as Text
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Exercise independently offset structure-of-arrays views, O(1) state
-- projection, and ComponentStateSequence's delegated JSON encoding.
main = do
    let components = U.slice 1 3
            (U.fromList [99,-1,2,3,99] :: U.Vector Int)
        states = U.slice 2 3
            (U.fromList [99,99,-1,4,5,99] :: U.Vector Int)
        values = U.zip components states
        sequence = ComponentStateSequence values
    print sequence
    print (U.toList (componentStates sequence))
    Text.putStrLn (encodeComponentStateSequence values)
    Text.putStrLn (JSON.encode sequence)
