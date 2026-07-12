{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (compressAlignment, compressAlignmentVarNonvar)
import Bio.Alphabet (dna)
import Compiler.Num
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.Text as Text
import Data.Tuple (snd)
import qualified Data.Vector.Unboxed as U
import Foreign.Vector (listToVector, vector_size)
import System.IO (print)

row name values = (Text.pack name, listToVector values)

-- Exercise both compression result layouts and derive zero and nonzero
-- logical lengths from their complete native Int owners.
main = do
    let alignment = [row "a" [0,1,0], row "b" [1,1,1]]
        (compressed, counts, mapping) = compressAlignment alignment
        (compressedVar, varCounts) =
            compressAlignmentVarNonvar alignment dna
        (_, emptyCounts, emptyMapping) = compressAlignment []
        (_, emptyVarCounts) = compressAlignmentVarNonvar [] dna

    print (U.toList counts)
    print (U.toList mapping)
    print (fmap (vector_size . snd) compressed)
    print (U.toList varCounts)
    print (fmap (vector_size . snd) compressedVar)
    print (U.length emptyCounts, U.length emptyMapping,
           U.length emptyVarCounts)
