module Bio.Alignment.Matrix where

import Bio.Sequence
import Bio.Alphabet
import Data.BitVector

data AlignmentMatrix

alignment_length :: AlignmentMatrix -> Int
builtin "Alignment:alignment_length" alignment_length 1

builtin_load_alignment :: Alphabet -> CPPString -> AlignmentMatrix
builtin "Alignment:load_alignment" builtin_load_alignment 2

load_alignment :: Alphabet -> String -> AlignmentMatrix
load_alignment alphabet filename = IOAction (\s -> (s, builtin_load_alignment alphabet (list_to_string filename)))

builtin_alignment_from_sequences :: Alphabet -> EVector Sequence -> AlignmentMatrix
builtin "Alignment:alignment_from_sequences" builtin_alignment_from_sequences 2

alignment_from_sequences :: Alphabet -> [Sequence] -> AlignmentMatrix
alignment_from_sequences a seqs = builtin_alignment_from_sequences a (list_to_vector seqs)

builtin_sequences_from_alignment :: AlignmentMatrix -> EVector Sequence
builtin "Alignment:sequences_from_alignment" builtin_sequences_from_alignment 1

sequences_from_alignment :: AlignmentMatrix -> [ Sequence ]
sequences_from_alignment a = list_from_vector $ builtin_sequences_from_alignment a

builtin_sequence_names :: AlignmentMatrix -> EVector CPPString
builtin "Alignment:sequence_names" builtin_sequence_names 1

sequence_names :: AlignmentMatrix -> [String]
sequence_names a = map unpack_cpp_string $ list_from_vector $ builtin_sequence_names a

builtin_reorder_alignment :: EVector Sequence -> AlignmentMatrix -> AlignmentMatrix
builtin "Alignment:reorder_alignment" builtin_reorder_alignment 2

reorder_alignment :: [String] -> AlignmentMatrix -> AlignmentMatrix
reorder_alignment names a = builtin_reorder_alignment names' a where names' = list_to_vector $ map pack_cpp_string names

builtin "Bits:alignment_row_to_presence_bitvector" builtin_alignment_row_to_bitvector 2
alignment_row_to_bitvector a row = BitVector $ builtin_alignment_row_to_bitvector a row

