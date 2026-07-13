{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (compressAlignment, compressAlignmentVarNonvar,
                      select_alignment_columns)
import Bio.Alignment.Matrix (alignment_from_sequences, alignment_length,
                             indices_from_alignment)
import Bio.Alphabet (dna)
import Compiler.Num
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.Text as Text
import Data.Tuple (snd)
import qualified Data.Vector.Unboxed as U
import Foreign.Vector (listToVector, vector_size, vectorToList)
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
        source = alignment_from_sequences dna
                     [(Text.pack "a", Text.pack "ACG"),
                      (Text.pack "b", Text.pack "TGA")]
        selected = select_alignment_columns source [2,0,2]
        selectedEmpty = select_alignment_columns source []

    print (U.toList counts)
    print (U.toList mapping)
    print (fmap (vector_size . snd) compressed)
    print (U.toList varCounts)
    print (fmap (vector_size . snd) compressedVar)
    print (U.length emptyCounts, U.length emptyMapping,
           U.length emptyVarCounts)
    print (alignment_length selected,
           fmap vectorToList (indices_from_alignment selected))
    print (alignment_length selectedEmpty)
