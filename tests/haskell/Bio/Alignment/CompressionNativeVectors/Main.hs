{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (compressAlignment, compressAlignmentVarNonvar,
                      fromLegacySequenceVector, select_alignment_columns,
                      toLegacySequenceVector)
import Bio.Alignment.Matrix (alignment_from_sequences, alignment_length,
                             indices_from_alignment)
import Bio.Alphabet (dna)
import Compiler.Num
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.Text as Text
import Data.Tuple (snd)
import qualified Data.Vector.Unboxed as U
import System.IO (print)

row name values = (Text.pack name, U.fromList values)

-- Protect direct legacy/native conversion, including sliced logical views, while exercising both
-- compression layouts; remove these checks when the nested legacy vector interfaces disappear.
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
        sliced = U.slice 1 3 (U.fromList [99,4,5,6,100] :: U.Vector Int)
        converted = fromLegacySequenceVector (toLegacySequenceVector sliced)
        convertedEmpty =
            fromLegacySequenceVector (toLegacySequenceVector (U.empty :: U.Vector Int))

    print (U.toList converted, U.length convertedEmpty)
    print (U.toList counts)
    print (U.toList mapping)
    print (fmap (U.length . snd) compressed)
    print (U.toList varCounts)
    print (fmap (U.length . snd) compressedVar)
    print (U.length emptyCounts, U.length emptyMapping,
           U.length emptyVarCounts)
    print (alignment_length selected,
           fmap U.toList (indices_from_alignment selected))
    print (alignment_length selectedEmpty)
