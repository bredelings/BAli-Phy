module Bio.Alignment.Class where


class Alignment a where
    alignmentLength :: a -> Int
    numSequences :: a -> Int
    sequenceLength :: a -> Int -> Int
